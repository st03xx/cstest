#include "stdafx.h"
#include <float.h>
#include <math.h>

#include <utilities/sha1.h>
#include "resource.h"

#include "konvert.h"

// für Script-Compilierung
#include <stcc/StFrame.h>
CStFrame    sframe;



// maxVARS = 256; wegen Compilerfehler Zahl direkt eingetragen
#if maxVar+maxRVar+maxXVar+2 >= 256
#error Overflow Variable Size 1 
#endif

#if offRVar+maxRVar+2 >= 256
#error Overflow Variable Size 2
#endif
 

void CExtender::reset(void)
{
  pBaseBefehl = NULL;
    Buffer.RemoveAll();
}

void CExtender::init( TBefehl*  pBefehl, CByteArray& rBuffer)
{
  pBaseBefehl = pBefehl;      // Zeiger auf aktuellen zu erweiternden Block 
  Buffer.Copy( rBuffer);
}

CString CExtender::check(void)
{
  CString ldstr, tmp;
  int   leng;

    if( Buffer.GetUpperBound() != -1 ) {
        // letztes Script nicht vollständig abgespeichert
      ldstr.LoadString(IDS_STRING56);
        leng = Buffer.GetSize();
        tmp.Format(_T("%s %d\n"), ldstr.GetString(), (leng+sizeof(TBefehl)-1)/sizeof(TBefehl));
    }
  reset();
  return tmp;
}

CString CExtender::convert(TBefehl* pBefehl)
{
  CString ldstr, tmp;
  int   leng, lnk;

    // Scriptbefehl im Schritt ?
  if( pBaseBefehl == NULL ) {
    ldstr.LoadString(IDS_STRING57);
      return ldstr;
  } else {
    // Linktest
    lnk = pBefehl - pBaseBefehl + 1;
    if( pBaseBefehl->Link < lnk ) {
      // Korrektur für ungenutzten Erweiterungsblock
      pBaseBefehl->Link = lnk ;
    }
    // Puffer noch gefüllt ?  
    if( Buffer.GetUpperBound() != -1 ) {
      // Es wird der GANZE Befehl überschrieben, auch die ersten 8 Byte des Basiscodes.
      // Der Gesamtcode wird durch das Linkbyte konsistent gehalten.
      leng = min(Buffer.GetSize(), sizeof(TBefehl)); 
      memcpy_s(pBefehl, sizeof(TBefehl), Buffer.GetData(), leng);
      Buffer.RemoveAt(0, leng);
    } else {      
      // Hier sollten wir nicht vorbeikommen!
      // Aber keine Fehlermeldung
//      ldstr.LoadString(IDS_STRING62);
        return ldstr;
    }
  }
  return ldstr;
}


CKonvert::CKonvert() 
{
 pbPC      = NULL; 
 pBefehl     = NULL;
 pPar          = NULL;
 pGPar       = NULL;
 pTPar       = NULL;
 pGTPar      = NULL;
 pVar          = NULL;
 pSchritte     = NULL;
 pParRef       = NULL;
 pSteps      = NULL;
 pRJmp       = NULL;
 m_rPTB        = 1.0f;
}

CKonvert::~CKonvert()
{
  clear();
}

int CKonvert::init(_TCHAR t, CUtf8File* _fref, CUtf8File* _ferr, int v, LPCTSTR _pSignale, LPCTSTR _lstname)
{
 clear();   // evtl. Puffer löschen

 CounterBefehl    = 0;
 CounterPar       = 0;
 CounterGPar    = 0;
 CounterSchritte  = 0;
 TotalSchritte    = 0;
 fehler           = 0;
 Zeile            = 0;
 Trenner          = t;
 variante     = v;
 pSignale         = _pSignale;
 pbPC = pBefehl   = new TBefehl[maxBefehle];
 pPar             = new float[maxPar+1];
 pGPar            = new float[maxPar+1];
 pTPar        = new TextPar[maxPar+1];
 pGTPar       = new TextPar[maxPar+1];
 pVar             = new UINT8[maxVARS];
 pSchritte        = new short[maxBefehle];
 pParRef          = new ParRef[maxParRef];
 pSteps       = new short[maxBefehle];
 pRJmp        = new RJmp[maxBefehle/2]; // Sollte genügen, da jeder Jmp eine Bedingung hat
 pPR              = pParRef;
 fref       = _fref;
 ferr       = _ferr;
 lstname          = _lstname;

 Extender.reset();

 BlockLineAddr.RemoveAll();

 // Speicher angelegt ?
 if(!pbPC || !pPar || !pGPar || !pTPar || !pGTPar || !pVar || !pSchritte || !pSteps || !pRJmp || !pParRef)
   return 0;

 // Parameterliste mit NAN (Not a Number) löschen
 memset(pPar,   0xff, sizeof(float)  * (maxPar+1));
 memset(pGPar,  0xff, sizeof(float)  * (maxPar+1));
 memset(pTPar,  0x00, sizeof(TextPar)* (maxPar+1));
 memset(pGTPar, 0x00, sizeof(TextPar)* (maxPar+1));
 // VariablenFlagliste löschen
 memset(pVar, 0, sizeof(UINT8)*maxVARS);
 // 0-Schritt für neuen CALL-Block
 pSchritte[0] = -1;

 return 1;
}

void CKonvert::clear()
{
 if( pbPC )       delete[] pbPC;
 if( pPar )       delete[] pPar;
 if( pGPar )      delete[] pGPar;
 if( pTPar )      delete[] pTPar;
 if( pGTPar )     delete[] pGTPar;
 if( pVar )       delete[] pVar;
 if( pSchritte )    delete[] pSchritte;
 if( pSteps )     delete[] pSteps;
 if( pRJmp )    delete[] pRJmp;
 if( pParRef )      delete[] pParRef;
 pbPC      = NULL; 
 pPar          = NULL;
 pGPar       = NULL;
 pTPar       = NULL;
 pGTPar      = NULL;
 pVar          = NULL;
 pSchritte     = NULL;
 pSteps      = NULL;
 pRJmp       = NULL;
 pParRef       = NULL;
 Comments.RemoveAll();
}

// Fehlernachricht für lexikalische Fehler des Prüfprogramms
void CKonvert::error(LPCTSTR s)
{
  CString errstr;

  if(!fehler) {
    errstr.LoadString(IDS_STRING1);
    ferr->WriteString(errstr);
  }
  if( variante & VARIANTE_INTERN_PPZ ) {
    errstr.Format(
            _T("%3d: %4d,%2d,%4d,%4d: %s"),
            ++fehler,             // Fehlernummer
            Zeile,                // Zeile
            spalte,               // Spalte
            Pruefschrittnummer &  0x0000FFFF, // PrüfSchritt
            Pruefschrittnummer >> 16,     // relative Blocknummer
            s
    );
  } else {
    errstr.Format(
            _T("%3d: %4d,%2d,%4d,%4d: %s"),
            ++fehler,               // Fehlernummer
            Zeile,                  // Zeile
            spalte,                 // Spalte
            Schrittnummer,          // Schritt
            relBlocknummer,     // relative Blocknummer
            s
    );
  }
  ferr->WriteString(errstr);
} 

// Fehlernachricht für semantische Fehler des Prüfprogramms
void CKonvert::semerr(int cb, int sn, LPCTSTR s)
{
  CString errstr;

  if(!fehler) {
    errstr.LoadString(IDS_STRING1);
    ferr->WriteString(errstr);
  }
  if( variante & VARIANTE_INTERN_PPZ ) {
    errstr.Format(  _T("%3d: %4d,%2d,%4d,%4d: %s"),
            ++fehler,                // Fehlernummer
            cb,                      // Zeile
            0,                       // Spalte
            Pruefschrittnummer &  0x0000FFFF, // PrüfSchritt
            Pruefschrittnummer >> 16,     // relative Blocknummer
            s
    );
  } else {
    errstr.Format(  _T("%3d: %4d,%2d,%4d,%4d: %s"),
            ++fehler,                // Fehlernummer
            cb,                      // Zeile
            0,                       // Spalte
            sn,                      // Schritt
            relBlocknummer,       // relative Blocknummer
            s
    );
  }
  ferr->WriteString(errstr);
}

void CKonvert::exterr(int sn, int bnr, LPCTSTR s)
{
  CString errstr;

  int relblocknummer     = relBlocknr[bnr];
  int pruefschrittnummer = Pruefschrittnr[bnr];

  if(!fehler) {
    errstr.LoadString(IDS_STRING1);
    ferr->WriteString(errstr);
  }
  if( variante & VARIANTE_INTERN_PPZ ) {
    errstr.Format(  _T("%3d: %4d,%2d,%4d,%4d: %s"),
            ++fehler,                // Fehlernummer
            0,                       // Zeile
            0,                       // Spalte
            pruefschrittnummer &  0x0000FFFF, // PrüfSchritt
            pruefschrittnummer >> 16,     // relative Blocknummer
            s
    );
  } else {
    errstr.Format(  _T("%3d: %4d,%2d,%4d,%4d: %s"),
            ++fehler,                // Fehlernummer
            0,                       // Zeile
            0,                       // Spalte
            sn,                      // Schritt
            relblocknummer,        // relative Blocknummer
            s
    );
  }
  ferr->WriteString(errstr);
}

void CKonvert::storeRef(_TCHAR typ, int pnr, int rbnr, int snr, int ps, int cc)
{
 if(pPR >= &pParRef[maxParRef-1]) {
  ldstr.LoadString(IDS_STRING2);
  ferr->WriteString(ldstr);
  exit(1);
 }
 if( variante & VARIANTE_INTERN_PPZ ) {
  snr   = ps &  0x0000FFFF; // PrüfSchritt
  rbnr  = ps >> 16;     // relative Blocknummer
 }
 pPR->t   = typ;
 pPR->pn  = pnr;
 pPR->rbn = rbnr;
 pPR->sn  = snr;
 pPR->bed = cc;
 pPR++;
}

// Überlese Tab und Space
// 0, leere Spalte, 1 = Parameter
int CKonvert::skip0(_TCHAR** ptr)
{
 _TCHAR*  line = *ptr;
  
 while( *line && _istspace(*line) ) line++;
 *ptr = line;
 
 return *line && (*line != Trenner);
}

// Eine Spalte überlesen
// maxlen in TCHAR, da String.GetBuffer() eine Länge in TCHAR entgegen nimmt.
int CKonvert::skip(LPTSTR  dest, int maxlen)
{
 LPTSTR p;
 int  l;

 spalte++;
 p = pLine;
 while(*pLine && (*pLine != Trenner)) pLine ++;
 if(!*pLine) {
   ldstr.LoadString(IDS_STRING8);
   error(ldstr);
   return 0;
 }
 l = pLine - p;
 if( l > maxlen-1 ) l = maxlen-1;
 pLine++;
 if( dest ) {
   memcpy_s(dest, maxlen*sizeof(_TCHAR), p, l*sizeof(_TCHAR));
   dest[l] = 0;
 }
 return 1;
}

// Kommas in der Zeile in Punkte wandeln
void CKonvert::k2p(LPTSTR p)
{
 while(*p) {
   if(*p == ',')
     *p = '.';
   p++;
 }
}

// Zeiger auf pLine setzen
// und führende Zeichen zwischen 0x01 und 0x20 überlesen
// _tcstoul macht Fehler bei dem String "  P1"
LPTSTR CKonvert::start(void)
{
 LPTSTR p;

 p = pLine;
 while(*p && (*(unsigned char*)p <= 0x20)) p++;
 return p;
}
// 1 = ok
int CKonvert::checkPtr(LPTSTR p)
{
 return _istspace(*p)  || (*p == Trenner) ;
}
// Bool einlesen und konvertieren
// Stringpointer hinter den Trenner stellen
Bool CKonvert::getBool(void)
{
 LPTSTR p;
 unsigned long l;

 p = start();
 skip();

 ldstr.LoadString(IDS_STRING4);
 l = _tcstoul(p, &p, 10);
 if( checkPtr(p) == 0 ) {
   error(ldstr);
   return 0;
 }
 if(l > 1L) {
   error(ldstr);
   return 0;
 }

 return (Bool)l;
}

unsigned long CKonvert::getExp1(LPTSTR &p)
{
  unsigned long x;

  x = getExp2(p);
  while( *p ) {
    while( *p == ' ' ) p++;
    if( *p == '+' ) {
      p++;
      x += getExp2(p);
    } else if( *p == '-' ) {
      p++;
      x -= getExp2(p);
    } else {
      break;
    }
  }
  return x;
}

unsigned long CKonvert::getExp2(LPTSTR &p)
{
  unsigned long x, y;

  x = getExp3(p);
  while( *p ) {
    while( *p == ' ' ) p++;
    if( *p == '*' ) {
      p++;
      x *= getExp3(p);
    } else if( *p == '/' ) {
      p++;
      y = getExp3(p);
      if( y == 0 ) {
        // Expression: /0\n
        CString tmp;
        tmp.LoadString(IDS_STRING95);
        error(tmp);
        return 0;
      }
      x /= y;
    } else {
      break;
    }
  }
  return x;
}

unsigned long CKonvert::getExp3(LPTSTR &p)
{
  unsigned long     x;

  while( *p == ' ' ) p++;

  // Klammerung
  if( *p == '(' ) {
    p++;
    x = getExp1(p);
    while( *p == ' ' ) p++;
    if( *p != ')' ) {
      // Expression: )\n
      CString tmp;
      tmp.LoadString(IDS_STRING96);
      error(tmp);
      return 0;
    }
    p++;
    return x;
  }

  // Vorzeichen
  if( *p == '-' ) {
    p++;
    return (unsigned long)(-(long)getExp3(p));
  }

  // Integerzahl
  return _tcstoul(p, &p, 10);
}


// Uint einlesen und konvertieren
// Stringpointer hinter den Trenner stellen
Uint CKonvert::getUint(void)
{
 _TCHAR     *p, *pb, buffer[256];
 unsigned long  l;
 int            i;

 p = start();
 skip();

 for(pb = buffer; *p != Trenner;) *pb++=*p++; *pb = 0;
 pb = buffer;
 if( _istalpha(*pb) ) {
    // Labelkonvertierung
    if( (i = KonvertLabel(buffer)) <= 0 ) {
      ldstr.LoadString(IDS_STRING59);
      error(ldstr);
      return 0;
    }
    return (Uint)i;
 }
 if( !_istdigit(*pb) ) {
  ldstr.LoadString(IDS_STRING34);
  error(ldstr);
  return 0;
 }
 l = getExp1(pb);
 if(*pb) {
  // Expression: ?\n
  CString tmp;
  tmp.LoadString(IDS_STRING97);
  error(tmp);
  return 0;
 }
 if(l > 0x0000ffffL) {
   ldstr.LoadString(IDS_STRING4);
   error(ldstr);
   return 0;
 }

 return (Uint)l;
}

// Udint einlesen und konvertieren
// Stringpointer hinter den Trenner stellen
Udint CKonvert::getUdint(void)
{
 _TCHAR     *p, *pb, buffer[256];
 unsigned long  l;
 int            i;

 p = start();
 skip();

 for(pb = buffer; *p != Trenner;) *pb++=*p++; *pb = 0;
 pb = buffer;
 if( _istalpha(*pb) ) {
    // Labelkonvertierung
    if( (i = KonvertLabel(buffer)) <= 0 ) {
      ldstr.LoadString(IDS_STRING59);
      error(ldstr);
      return 0;
    }
    return (Udint)i;
 }
 if( !_istdigit(*pb) ) {
  ldstr.LoadString(IDS_STRING34);
  error(ldstr);
  return 0;
 }
 l = getExp1(pb);
 if(*pb) {
  // Expression: ?\n
  CString tmp;
  tmp.LoadString(IDS_STRING97);
  error(tmp);
  return 0;
 }

 return (Udint)l;
}

// Uint einlesen und konvertieren
// Stringpointer hinter den Trenner stellen
// Wie getUint, aber ohne Fehlertest, Label und Expression
Uint CKonvert::getHelp(int index)
{
 _TCHAR     *p, *pb, buffer[256];
 unsigned long  l;

 p = start();
 skip();

 for(pb = buffer; *p != Trenner;) *pb++=*p++; *pb = 0;
 pb = buffer;
 if( index == 7 )
  strHelp7 = buffer;  // Help 7 bleibt
 if( !_istdigit(*pb) ) {
  l = 0;
 } else {
   l = _tstoi( pb );
 }

 return (Uint)l;
}

// V01 bis V99 einlesen und konvertieren
// R01 bis R99 einlesen und konvertieren
// X01 bis X19 einlesen und konvertieren
// Stringpointer hinter den Trenner stellen
Uint CKonvert::getVal1V(void)
{
 LPTSTR        p;
 unsigned long l;

 p = start();
 skip();


 if(_totlower(*p) == 'v') {
   // V überlesen
   p++;
  l = _tcstoul(p, &p, 10);
  if( checkPtr(p) == 0 ) {
      ldstr.LoadString(IDS_STRING4);
     error(ldstr);
     return 0;
   }
   if(l > (unsigned int)maxVar) {
     ldstr.LoadString(IDS_STRING5);
     error(ldstr);
     return 0;
   }

   // Variablenquelle für Referenzliste speichern
  storeRef('v', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);
 }
 else if( (_totlower(*p) == 'r') && ( variante & VARIANTE_OPTION_01) ) {
     // R überlesen
     p++;
   l = _tcstoul(p, &p, 10);
   if( checkPtr(p) == 0 ) {
     error(ldstr);
     return 0;
   }
   if(l > (unsigned int)maxRVar) {
     ldstr.LoadString(IDS_STRING5);
     error(ldstr);
     return 0;
   }

   // Variablenquelle für Referenzliste speichern
   storeRef('r', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);
   // Index auf externe Variable umrechnen
   l += offRVar;
 }
 else if( _totlower(*p) == 'x' ) {
     // X überlesen
     p++;
   l = _tcstoul(p, &p, 10);
   if( checkPtr(p) == 0 ) {
     error(ldstr);
     return 0;
   }
   if(l > (unsigned int)maxXVar) {
     ldstr.LoadString(IDS_STRING5);
     error(ldstr);
     return 0;
   }

   // Variablenquelle für Referenzliste speichern
   storeRef('x', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);
   // Index auf externe Variable umrechnen
   l += offXVar;
 }
 else {
  ldstr.LoadString(IDS_STRING48);
  error(ldstr);
  return 0;
 }
 return (unsigned short)l;
}



// "keine", V01 bis V99, R01 bis R99 oder X01 bis X99 einlesen und konvertieren
// Stringpointer hinter den Trenner stellen
Uint CKonvert::getStore(void)
{
 LPTSTR        p;
 unsigned long l;

 p = start();
 skip();

 ldstr.LoadString(IDS_STRING4);

 if(_totlower(*p) == 'k')
   // Keine
   return 0;

 if(_totlower(*p) == 'v') {
     // V überlesen
     p++;
   l = _tcstoul(p, &p, 10);
     if( checkPtr(p) == 0 ) {
     error(ldstr);
     return 0;
   }
   if(l > (unsigned int)maxVar) {
     ldstr.LoadString(IDS_STRING5);
     error(ldstr);
     return 0;
   }

   // Variablenquelle für Referenzliste speichern
   storeRef('V', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);
 }
 else if( (_totlower(*p) == 'r') && ( variante & VARIANTE_OPTION_01) ) {
     // V überlesen
     p++;
   l = _tcstoul(p, &p, 10);
     if( checkPtr(p) == 0 ) {
     error(ldstr);
     return 0;
   }
   if(l > (unsigned int)maxRVar) {
     ldstr.LoadString(IDS_STRING5);
     error(ldstr);
     return 0;
   }

   // Variablenquelle für Referenzliste speichern
   storeRef('R', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);
   // Index auf externe Variable umrechnen
   l += offRVar;
 }
 else if( _totlower(*p) == 'x' ) {
     // X überlesen
     p++;
   l = _tcstoul(p, &p, 10);
   if( checkPtr(p) == 0 ) {
     error(ldstr);
     return 0;
   }
   if(l > (unsigned int)maxXVar) {
     ldstr.LoadString(IDS_STRING5);
     error(ldstr);
     return 0;
   }

   // Variablenquelle für Referenzliste speichern
   storeRef('X', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);
   // Index auf externe Variable umrechnen
   l += offXVar;
 }
 else {
  // einfache Konstante von Parametriermodulen
  return (unsigned short)_tcstoul(p, &p, 10);
 }
 return (unsigned short)l;
}


// Variable einlesen
// Konstante -> Konvertieren (Zeitkonstanten)
// Pxx Aus Parametertabelle holen, Fehler falls Parameter nicht existiert 
// Gxx Aus Globaler Parametertabelle holen, Fehler falls Parameter nicht existiert 
// Vxx Variablekennung setzen
// Rxx Variablekennung setzen
// Xxx Variablenkennung setzen
// Stringpointer hinter den Trenner stellen
Real CKonvert::getVar(void)
{
 const int maxbuf = 256;
 _TCHAR   *p, *pe, *pt;
 long   l, ms;
 _TCHAR buf[maxbuf];
 double d;

 vtype = 'c';
 p = start();
 skip();

 // Leere Spalte ?
 if(*p == Trenner) {
   return 0.0f;
 }

 // Keine ?
 if(_totlower(*p) == 'k') {
   return 0.0f;
 }

 // Variable ?
 if(_totlower(*p) == 'v') {
   vtype = 'v';
   p++;
   l = _tcstoul(p, &p, 10);
   if((l == 0) || (l > maxVar)) {
    // Vxx\n
    CString tmp;
    tmp.LoadString(IDS_STRING98);
    error(tmp);
    return 0.0f;
   }
   // Variablennutzung für Referenzliste speichern
   storeRef('v', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);

   // Eintragen
   return setNAN((unsigned short)l);
 }
 else if( (_totlower(*p) == 'r') && ( variante & VARIANTE_OPTION_01) ) {
   vtype = 'r';
   p++;
   l = _tcstoul(p, &p, 10);
   if((l == 0) || (l > maxVar)) {
    // Rxx\n
    CString tmp;
    tmp.LoadString(IDS_STRING98);
    error(tmp);
    return 0.0f;
   }
   // Variablennutzung für Referenzliste speichern
   storeRef('r', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);

   // Eintragen
   return setNAN((unsigned short)l+offRVar);
 }

 if( _totlower(*p) == 'x' ) {
   vtype = 'v';
   p++;
   l = _tcstoul(p, &p, 10);
   if((l == 0) || (l > maxXVar)) {
    // Xxx\n
    CString tmp;
    tmp.LoadString(IDS_STRING99);
    error(tmp);
     return 0.0f;
   }
   // Variablennutzung für Referenzliste speichern
   storeRef('x', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);

   // Eintragen
   return setNAN((unsigned short)l+offXVar);
 }
 // Parameter ?
 if(_totlower(*p) == 'p') {
  ldstr.LoadString(IDS_STRING6);  
  vtype = 'p';
  p++;
  l = _tcstoul(p, &p, 10);
  // Parameternutzung für Referenzliste speichern
  storeRef('p', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);

  if(CounterPar == 0) {
    // Keine Parameterliste übergeben
    return 0.0f;
  }
  if((l == 0) || (l > CounterPar)) {
    _stprintf_s(buf, maxbuf, ldstr, (int)l);
    error(buf);
    return 0.0f;
  }
  if(isNAN(&pPar[(unsigned short)l])) {
    _stprintf_s(buf, maxbuf, ldstr, (int)l);
    error(buf);
    return 0.0f;
   }
   // Eintragen
   return pPar[(unsigned short)l];
 }
 
 // Globale Parameter ?
 if( _totlower(*p) == 'g' ) {
  ldstr.LoadString(IDS_STRING52); 
  vtype = 'g';
  p++;
  l = _tcstoul(p, &p, 10);
  // Parameternutzung für Referenzliste speichern
  storeRef('g', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);

  if(CounterGPar == 0) {
    // Keine Parameterliste übergeben
    return 0.0f;
  }
  if((l == 0) || (l > CounterGPar)) {
    _stprintf_s(buf, maxbuf, ldstr, (int)l);
    error(buf);
    return 0.0f;
  }
  if(isNAN(&pGPar[(unsigned short)l])) {
    _stprintf_s(buf, maxbuf, ldstr, (int)l);
    error(buf);
    return 0.0f;
   }
   // Eintragen
   return pGPar[(unsigned short)l];
 }
 
 // Konstante
 k2p(p);
 pe = _tcschr(p , Trenner) - 1;
 pt = _tcspbrk(p, _T("DHMSdhms") );
 if( pt && (pt <= pe)  ) {
  // Zeitkonstante
  d  = 0.0;
  ms = _istdigit( *pe ) ? 1000l : 1l;
  while(*p != Trenner) {
    long l  = _tcstoul(p, &p, 10);
    switch( *p ) {
    case 'd':
    case 'D': l *= 86400*ms; d += l; break;
    case 'h':
    case 'H': l *=  3600*ms; d += l; break;
    case 'm':
    case 'M': l *=    60*ms; d += l; break;
    case 's':
    case 'S': l *=     1*ms; d += l; break;
    default :          d += l;
    }
    while( !_istdigit(*p) && (*p != Trenner) ) p++;
  }
 } else {
  d = _tcstod(p, &p);
  if( checkPtr(p) == 0 ) {
    ldstr.LoadString(IDS_STRING4);  
    error(ldstr);
    return 0.0f;
  } 
 }

 if(fabs(d) >= FLT_MAX) {
  ldstr.LoadString(IDS_STRING7);  
  error(ldstr);
  return 0.0f;
 }
 return (float)d;
}

CString CKonvert::getTPar(CString& in, int maxLen)
{
  const int maxbuf = 256;
  CString   out, ldstr, tmp;
  LPTSTR      p;
  long    l;
  _TCHAR    buf[maxbuf];
  _TCHAR    par;

  in.TrimLeft();
  in.TrimRight();
  out = in;
  if( (in.GetLength() >= 2) && in[0] == '"' ) {
    // Textkonstante
    out = in.Mid(1, in.GetLength()-2);
  } else {
    // Textparameter
    p = (LPTSTR)in.GetBuffer(0);
    par = _totlower(*p);
    if( par == 'p' ) {
      p++;
      l = _tcstoul(p, &p, 10);
      if(CounterPar == 0) {
        // Keine Parameterliste übergeben
        return out;
      }
      if((l == 0) || (l > CounterPar) || (pTPar[(unsigned short)l].text[0] == '\0') ) {
        ldstr.LoadString(IDS_STRING6);  
        _stprintf_s(buf, maxbuf, ldstr, (int)l);
        error(buf);
        return out;
      }
      out = pTPar[(unsigned short)l].text;
      // Parameternutzung für Referenzliste speichern
      storeRef('p', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);
    } else if( par == 'g' ) {
      p++;
      l = _tcstoul(p, &p, 10);
      if(CounterGPar == 0) {
        // Keine Parameterliste übergeben
        return out;
      }
      if((l == 0) || (l > CounterGPar) || (pGTPar[(unsigned short)l].text[0] == '\0') ) {
        ldstr.LoadString(IDS_STRING114);  
        _stprintf_s(buf, maxbuf, ldstr, (int)l);
        error(buf);
        return out;
      }
      out = pGTPar[(unsigned short)l].text;
      // Parameternutzung für Referenzliste speichern
      storeRef('g', l, relBlocknummer, Schrittnummer, Pruefschrittnummer);
    } else {
      ldstr.LoadString(IDS_STRING72); 
      error(ldstr);
      return out;
    }
  }
  if( out.GetLength() > maxLen ) {
    ldstr.LoadString(IDS_STRING71);
    tmp.Format(_T(" (%d)"), out.GetLength() );
    error(ldstr+tmp);
  }

  return out;
}


// Kennung Variable (keine Konstante) setzen
float CKonvert::setNAN(unsigned short u)
{
 long l;

 l = 0xffff0000L | u;
 return *(float*)&l;
}

// Eine Variable (keine Konstante) ?
int CKonvert::isNAN(float* f)
{
 return (*(unsigned long*)f) > 0xffff0000L;
}

// Eine float-Zahl (Konstante) ?
int CKonvert::isVal(float f)
{
 return (*(unsigned long*)&f) < 0xffff0000L;
}

// Index der float-Zahl
int CKonvert::getNAN(float* f)
{
 return (int)((*(unsigned long*)f) & 0x0000ffffL);
}

// Parameter konvertieren
int CKonvert::konPar(LPCTSTR line, int counter, bool global)
{
 double   d;
 LPTSTR   p;
 int    l;
 CString  s;

 if(*line && (*line != '"') ) {
  d = _tcstod(line, &p);
  if( _istgraph(*p) ) {
    ldstr.LoadString(IDS_STRING9);
    _stprintf_s(buffer, maxbuffer, ldstr, counter);
    fehler++;
    ferr->WriteString((LPTSTR)buffer);
    return 0;
  }
  if(fabs(d) >= FLT_MAX) {
    ldstr.LoadString(IDS_STRING10);
    _stprintf_s(buffer, maxbuffer, ldstr, counter);
    ferr->WriteString((_TCHAR*)buffer);
    fehler++;
    return 0;
  }
  if( global)
    pGPar[counter] = (float)d;   
  else
    pPar[counter]  = (float)d;
 }
 // else:   // NAN-Kennung ist schon gesetzt

 // Textparameter ?
 // ***** Als ASCII oder Unicode gespeichert *****
 if( *line && (*line == '"') ) {
  p = _tcschr((LPTSTR)line+1, '"');
  l = p - line + 1;
  p = _tcschr((LPTSTR)line, ';');
  // ; im Text                       Ende-"                  Länge
  if( ((p != NULL) && (p-line<l)) || (line[l-1] != '"') ||  (l > 66) ) {
    ldstr.LoadString(IDS_STRING9);
    _stprintf_s(buffer, maxbuffer, ldstr, counter);
    fehler++;
    ferr->WriteString(buffer);
    return 0;
  }
  if( global)
    _tcsncpy_s(pGTPar[counter].text, maxTextLen, line+1, l-2);
  else
    _tcsncpy_s(pTPar[counter].text,  maxTextLen, line+1, l-2);
 }
 // else  Feld mit Nullen initialisiert

 // Stets ist nur einer der zwei Parameter initialisiert!
 return 1;
}


// Parameter einlesen
int CKonvert::readPar(LPCTSTR line)
{
 k2p((LPTSTR)line);
 // Leerzeile oder Kommentar ? => Parameter nicht gesetzt
 if( !skip0( (LPTSTR*)&line ) ) {
   // Kommentar überlesen; werden nicht mitgezählt!
   return 1;
 }

 if(++CounterPar > maxPar) {
// Fehler abgeschalten, da Access weitere Datensätze evt. anfügt.
//   error("Zu viele Parameter\n");
//   fehler++;
   return 1;
 }

 return konPar(line, CounterPar, false);
}

// Globale Parameter einlesen
int CKonvert::readGPar(LPCTSTR line)
{
 k2p((LPTSTR)line);
 // Leerzeile oder Kommentar ? => Parameter nicht gesetzt
 if( !skip0( (LPTSTR*)&line ) ) {
   // Kommentar überlesen; werden nicht mitgezählt!
   return 1;
 }

 if(++CounterGPar > maxPar) {
// Fehler abgeschalten, da Access weitere Datensätze evt. anfügt.
//   error("Zu viele Parameter\n");
//   fehler++;
   return 1;
 }

 return konPar(line, CounterGPar, true);
}

// LabelString auslesen
void CKonvert::setLabel(LPCTSTR s)
{
 LPTSTR buffer;
 int  i, pos;

 pLine  = (LPTSTR)s;
 for(i = 0; i < 29; i++) skip();  // Suche Help1
 buffer = LabelStr.GetBuffer(1024);
 skip(buffer, 1024);
 TotalSchritte = _tstoi(buffer);
 LabelStr.ReleaseBuffer(-1);
 LabelStr.MakeLower();
 LabelStr += CString(':');
 pos = LabelStr.Find(':');
 LabelStr = LabelStr.Mid(pos);
}

void CKonvert::konvertTime(float& x, bool inverse)
{
  if( isVal(x) ) {
    if( !inverse )
      x *= m_rPTB;
    else
      x /= m_rPTB;
  }
}


/*BEGIN_WIZARD_FELD*/

// Einen Befehl konvertieren
int CKonvert::readBefehl(LPCTSTR s)
{
 unsigned short blockType;
 _TCHAR         v6type;
 int            i, leng;
 unsigned int help[18]; // Index 1 .. 9
 CString    ldkg, tmp, strAnzeigetext, strMaske;
 char     asciiAnzeigetext[80];

 ldkg.LoadString(IDS_STRING19);

 Zeile++;       // Damit die Zeilenzählung stimmt.
 pLine  = (LPTSTR)s;
 spalte = 0;

 //$$= Blocknummer
 skip();

 //$$= Block_im_Schritt
 relBlocknummer = getUint();
 relBlocknr[pBefehl - pbPC] = relBlocknummer;
 if(relBlocknummer > maxrBlk) {
   ldstr.LoadString(IDS_STRING11);
   error(ldstr);
 }

 //$$= Schritt
 Schrittnummer = getUint();

 //$$= Prüfschritt
 Pruefschrittnummer = getUdint();
 Pruefschrittnr[pBefehl - pbPC] = Pruefschrittnummer;
 if(relBlocknummer > maxrBlk) {
   ldstr.LoadString(IDS_STRING11);
   error(ldstr);
 }

 //$$= Block_ID
 blockType = getUint();
 if((blockType == 0) || (blockType >= bMax))
 {
   ldstr.LoadString(IDS_STRING12);
   error(ldstr);
 }
 else {
   pBefehl->Modul = (TBlock)blockType;
 }

 //$$= Block_name
 skip();

 //$$= Kommentar
 skip(strAnzeigetext.GetBuffer(4096), 4095);
 strAnzeigetext.ReleaseBuffer(-1);
 Comments.Add( strAnzeigetext.Left(16) );

 //$$= Kommentar_Fehler
 skip(strAnzeigetext.GetBuffer(4096), 4095);
 strAnzeigetext.ReleaseBuffer(-1);

 // Val 1 - 4
 pBefehl->Val1   = (blockType == bKonstantenfeld) ? getVal1V() : getUdint();//$$= Val1
 pBefehl->Val2   = getUdint();      //$$= Val2
 pBefehl->Val3   = getUdint();      //$$= Val3
 pBefehl->Val4   = getUdint();      //$$= Val4

 // Var 1 - 9
 pBefehl->Var1   = getVar();      //$$= Var1
 pBefehl->Var2   = getVar();      //$$= Var2
 pBefehl->Var3   = getVar();      //$$= Var3
 pBefehl->Var4   = getVar();      //$$= Var4
 pBefehl->Var5   = getVar();      //$$= Var5
 pBefehl->Var6   = getVar();      //$$= Var6
 v6type          = vtype;
 pBefehl->Var7   = getVar();      //$$= Var7
 pBefehl->Var8   = getVar();      //$$= Var8
 pBefehl->Var9   = getVar();      //$$= Var9

 // Store
 pBefehl->Store  = getStore();        //$$= Store

 // B1 - 4
 pBefehl->B1  = getBool();      //$$= B1
 pBefehl->B2  = getBool();      //$$= B2
 pBefehl->B3  = getBool();      //$$= B3
 pBefehl->B4  = getBool();      //$$= B4

 // Doc, Err, Save lesen
          getBool();      //$$= Doc
 pBefehl->Err = getBool();      //$$= Err
          getBool();      //$$= Save  
 // Link auf Folgeblock setzen
 pBefehl->Link  = 1;

 // Help1 - Help9
 // Fastchannels , Pack
 for(i = 1; i <= 9; i++) help[i] = getHelp(i);

 pBefehl->Intern  = 0;
 if (!(variante & VARIANTE_INTERN_UMS))
 {
   // Spezielle Behandlung einzelner Module
   switch (blockType) {
   case bVorgabe:
     // Schrittnummer speichern
     pBefehl->Intern = Schrittnummer;
     CounterSchritte = Schrittnummer;
     // Blocknummer speichern
     pSchritte[Schrittnummer] = CounterBefehl;
     // Err-Flag unbenutzt im Editor
     // Zur Sicherheit gelöscht.
     pBefehl->Err = 0;
     // Schritttyp speichern (nur bei bStart benötigt)
     // Schritttyp = pBefehl->B2;
     // Prüfschritt mode 256 sichern
     // pBefehl->Res1  = Pruefschrittnummer;
     // Fastchannel nicht mehr benutzt!

     // Puffer leer ?
     tmp = Extender.check();
     if (!tmp.IsEmpty()) error(tmp);

     // Maskendefinition
     if (variante & VARIANTE_OPTION_02) {
       if (help[6] != 0) {
         pBefehl->Store = 0xFFFF;
       }
     }
     break;

   case bToleranz_Differenz:
   case bLeistungspruefung_Differenz:
     // Messwert 2
     if (v6type == 'c')
       pBefehl->Val2 = (Udint)pBefehl->Var6;
     else
       pBefehl->Val2 = 0;
     break;

   case bArithmetikblock_Verknuepfungen:
   case bArithmetikblock_Logik:
     // Operand 2 von Var2 auf Var5 kopieren
     pBefehl->Var5 = pBefehl->Var2;
     break;

   case bKorrekturblock_Messsignal:
     if (!pBefehl->Val2) {
       ldstr.LoadString(IDS_STRING13);
       error(ldstr);
     }
     break;

   case bKorrekturblock_Vorgabesignal:
     if (!pBefehl->Val3) {
       ldstr.LoadString(IDS_STRING13);
       error(ldstr);
     }
     break;

   case bUnterprogrammruf:
     // Maskendefinition
     if (variante & VARIANTE_OPTION_02) {
       // Callquelle für Referenzliste speichern
       if (pBefehl->Val1 == pBefehl->Val2) {
         storeRef('c', (short)pBefehl->Val1, relBlocknummer, Schrittnummer, Pruefschrittnummer, 3);
       }
       else {
         storeRef('c', (short)pBefehl->Val1, relBlocknummer, Schrittnummer, Pruefschrittnummer, 1);
         storeRef('c', (short)pBefehl->Val2, relBlocknummer, Schrittnummer, Pruefschrittnummer, 2);
       }
     }
     else {
       // Callquelle für Referenzliste speichern
       storeRef('c', (short)pBefehl->Val1, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     }
     break;
     /*
       case bParametrierblock_Messkreis:
       case bParametrierblock_Regelkreis:
       case bParametrierblock_Zeitgeber:
       if( (variante & VARIANTE_START) && (pBefehl->Val2 > 0) && (pBefehl->Val2 < maxBefehle)) {
         // Interruptquelle für Referenzliste speichern
         storeRef('c', (short)pBefehl->Val2, relBlocknummer, Schrittnummer, Pruefschrittnummer);
       }
       break;
     */

   case bProgrammabbruch:
     if (pBefehl->Var1 == 0) {
       // KEINE
       pBefehl->Val4 = 0;
       break;
     }
     // Mapping nur Vxx erlaubt
     if (isVal(pBefehl->Var1)) {
       ldstr.LoadString(IDS_STRING48);
       error(ldstr);
       break;
     }
     // Variablenkennung in Real-Konstanten wandeln
     pBefehl->Val4 = getNAN(&pBefehl->Var1);
     // Variablenquelle für Referenzliste speichern
     storeRef('V', pBefehl->Val4, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     storeRef('V', pBefehl->Val4 + 1, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     storeRef('V', pBefehl->Val4 + 2, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     break;

   case bArithmetikblock_Statistik:
     // Vxx oder Keine erlaubt
     // Variablenkennung in Real-Konstanten wandeln
     pBefehl->Var1 = (float)getNAN(&pBefehl->Var1);
     pBefehl->Var2 = (float)getNAN(&pBefehl->Var2);
     pBefehl->Var5 = (float)getNAN(&pBefehl->Var5);
     pBefehl->Var6 = (float)getNAN(&pBefehl->Var6);
     pBefehl->Var7 = (float)getNAN(&pBefehl->Var7);
     // Variablenquelle für Referenzliste speichern
     if (pBefehl->Var1)
       storeRef('V', (int)pBefehl->Var1, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var2)
       storeRef('V', (int)pBefehl->Var2, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var5)
       storeRef('V', (int)pBefehl->Var5, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var6)
       storeRef('V', (int)pBefehl->Var6, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var7)
       storeRef('V', (int)pBefehl->Var7, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     break;

     // ***** Nur für ASCII-Zeichen im SPC *****
   case bAnzeige:
     if (pBefehl->B1 && (pBefehl->B2 || pBefehl->B3)) {
       ldstr.LoadString(IDS_STRING23);
       error(ldstr);
     }
     if (pBefehl->B1) {
       // Val1 und 3 verdichten
       pBefehl->Val1 = (pBefehl->Val1 % 2000) + (pBefehl->Val3 * 2000);
       // Text einsortieren
       leng = 12 * sizeof(float);
       strAnzeigetext = getTPar(strAnzeigetext, leng);
       // Win - > Dos Zeichenkonvertierung und move 
       if (CUniConversion::str_to_ascii(
         strAnzeigetext,    //LPCSTR lpszSrc, 
         asciiAnzeigetext,  //LPTSTR lpszDst, Länge unverändert
         leng + 1       //DWORD cchDstLength + Endezeichen 0x00
       ) < 0) {
         ldstr.LoadString(IDS_STRING71);
         error(ldstr);
       }
       else memcpy((char*)&pBefehl->Val2, asciiAnzeigetext, leng);  // Länge ohne Endezeichen 0x00
     }
     else {
       // Meßwert 2
       if (v6type == 'c')
         pBefehl->Val2 = (Udint)pBefehl->Var6;
       else
         pBefehl->Val2 = 0;
     }
     // Store von Ziel in Quelle wandeln; 
     // Wenn ein Eintrag erfolgte (!KEINE), dann steht er in pPR[-1]
     if (pBefehl->Store) { // nicht KEINE
       pPR[-1].t = _totlower(pPR[-1].t); // in Kleinbuchstabe wandeln
     }
     break;

   case bKonstantenfeld_Erweiterung:
     if ((pBefehl[-1].Modul != bKonstantenfeld) && (pBefehl[-1].Modul != bKonstantenfeld_Erweiterung)) {
       ldstr.LoadString(IDS_STRING54);
       error(ldstr);
     }

     break;
   case bScript:
   {
     int         rc, pos;
     _TCHAR   ch;

     // Puffer leer ?
     tmp = Extender.check();
     if (!tmp.IsEmpty()) error(tmp);
     // compilieren für Baustein
     // String zurückwandeln
     strAnzeigetext.Replace(_T("\\sk"), _T(";"));
     strAnzeigetext.Replace(_T("\\rn"), _T("\r\n"));
     ReplaceLabel(strAnzeigetext);
     sframe.clrZcode();
     // Listfile vorbereiten
     sframe.setPTB(m_rPTB);
     rc = sframe.compile(strAnzeigetext,  //const char* szsource, 
       0,               //warn off 
       TotalSchritte,
       pPar,
       pGPar,
       pSchritte,
       pSignale,
       lstname          //const char* Listfilename
     );

     if (rc) {
       int      cs1b = sizeof(TBefehl) - BefehlBaseSize;  // CodeSizeFirstBefehl
       int      count;
       CByteArray ScriptBuffer;

       ScriptBuffer.Copy(sframe.getZcode());
       leng = min(ScriptBuffer.GetSize(), cs1b);
       count = (ScriptBuffer.GetSize() - leng + sizeof(TBefehl) - 1) / sizeof(TBefehl);
       memcpy_s(&pBefehl->Val1, cs1b, ScriptBuffer.GetData(), leng);
       pBefehl->Link = 1 + count;
       ScriptBuffer.RemoveAt(0, leng);
       Extender.init(pBefehl, ScriptBuffer);

       // Benutzte Input-Variable (Quellen) in Referenzen eintragen
       tmp = sframe.getVariable(true);
       while (!tmp.IsEmpty()) {
         ch = _totlower(tmp[1]);
         tmp = tmp.Mid(2);       // @V löschen
         i = _tstoi(tmp);
         pos = tmp.Find('@');
         if (pos == -1) tmp.Empty();
         else            tmp = tmp.Mid(pos);
         // Variablenquelle für Referenzliste speichern
         storeRef(ch, i, relBlocknummer, Schrittnummer, Pruefschrittnummer);
       }
       // Benutzte Output-Variable (Senken) in Referenzen eintragen
       // Beschriebene Variable setzen
       tmp = sframe.getVariable(false);
       while (!tmp.IsEmpty()) {
         ch = toupper(tmp[1]);
         tmp = tmp.Mid(2);       // @V, @X, @T löschen
         i = _tstoi(tmp);
         pos = tmp.Find('@');
         if (pos == -1) tmp.Empty();
         else            tmp = tmp.Mid(pos);
         // Variablenquelle für Referenzliste speichern
         storeRef(ch, i, relBlocknummer, Schrittnummer, Pruefschrittnummer);
         // global: wurde beschrieben
         // lokal : wie oft    "     ; 1  
       }
       if (lstname) {
         CString     tmpFilename;
         tmpFilename = lstname;
         tmpFilename += CString("lst"); // kein ".", da er schon vorhanden ist!
         if (!readTmpLstFile(tmpFilename, CounterBefehl)) {
           ldstr.LoadString(IDS_STRING65);
           error(ldstr);
         }
       }
     }
     else {
       error(sframe.getErrMsg());
     }
   }
   break;

   case bScript_Erweiterung:
     tmp = Extender.convert(pBefehl);
     if (!tmp.IsEmpty())
       error(tmp);
     break;

   case bFrontEnd1:
     // VARIANTE_PACK_FE1 entfernt
     break;
   case bFrontEnd2:
     break;
   case bFrontEnd3:
     break;
   case bFrontEnd4:
     break;
   case bFrontEnd5:
     break;
   case bFrontEnd6:
     break;
   case bFrontEnd7:
     break;
   case bFrontEnd8:
     break;

   case bSchnelle_Leistungspruefung:
   case bSchnelle_Toleranz:
     ldstr.LoadString(IDS_STRING3);
     error(ldstr);
     break;

     /*
     Alle Kanäle sind schnell!
     case bSchwellzeit:
     case bToleranzpruefung_Schwellzeit:
       // Schneller Analogkanäle <= 16,32
       if( (pBefehl->Val1 > fastmax) || (pBefehl->Val2 > fastmax) ) {
         ldstr.LoadString(IDS_STRING64);
         error(ldstr);
       }
       break;
     */

   case bArchivierung:
     // Meßwert 2
     if (v6type == 'c')
       pBefehl->Val2 = (Udint)pBefehl->Var6;
     else
       pBefehl->Val2 = 0;
     break;

   case bArchivauswertung:
     // Variablenkennung in Real-Konstanten wandeln
     pBefehl->Var6 = (float)getNAN(&pBefehl->Var6);
     pBefehl->Var7 = (float)getNAN(&pBefehl->Var7);
     // Variablenquelle für Referenzliste speichern
     if (pBefehl->Var6 != 0)
       storeRef('V', (int)pBefehl->Var6, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var7 != 0)
       storeRef('V', (int)pBefehl->Var7, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     break;

   case bAbfrage:
     if (strAnzeigetext.GetLength() > 1) {
       if (strAnzeigetext[1] == '>')
         storeRef('t', pBefehl->Val1, relBlocknummer, Schrittnummer, Pruefschrittnummer);
       else
         storeRef('T', pBefehl->Val1, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     }
     break;

   case bAbfrageValue:
     // Vxx oder Keine erlaubt
     // Variablenkennung in Real-Konstanten wandeln
     // 0 steht für KEINE oder leeres Feld
     pBefehl->Var1 = (float)getNAN(&pBefehl->Var1);
     pBefehl->Var2 = (float)getNAN(&pBefehl->Var2);
     pBefehl->Var3 = (float)getNAN(&pBefehl->Var3);
     pBefehl->Var4 = (float)getNAN(&pBefehl->Var4);
     pBefehl->Var5 = (float)getNAN(&pBefehl->Var5);
     pBefehl->Var6 = (float)getNAN(&pBefehl->Var6);
     pBefehl->Var7 = (float)getNAN(&pBefehl->Var7);
     pBefehl->Var8 = (float)getNAN(&pBefehl->Var8);
     pBefehl->Var9 = (float)getNAN(&pBefehl->Var9);
     // Variablenquelle für Referenzliste speichern
     if (!pBefehl->B1) break;
     if (pBefehl->Var1) storeRef('V', (int)pBefehl->Var1, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var2) storeRef('V', (int)pBefehl->Var2, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var3) storeRef('V', (int)pBefehl->Var3, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var4) storeRef('V', (int)pBefehl->Var4, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var5) storeRef('V', (int)pBefehl->Var5, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var6) storeRef('V', (int)pBefehl->Var6, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var7) storeRef('V', (int)pBefehl->Var7, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var8) storeRef('V', (int)pBefehl->Var8, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Var9) storeRef('V', (int)pBefehl->Var9, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     break;

   case bAbfrageDlg:
     break;

   case bAbfrageSignal:
     break;

     // ***** Nur für ASCII-Zeichen im SPC *****
   case bTextverarbeitung:
     if ((pBefehl->Val1 != 0) && (pBefehl->Val1 != 2) && !(pBefehl->Val2 | pBefehl->Val3)) {
       // 2 Textkonstanten angefordert, bei CONCAT, EQ, CMP
       ldstr.LoadString(IDS_STRING70);
       error(ldstr);
     }
     if ((pBefehl->Store != 0) && (pBefehl->Val1 >= 3)) {
       // Vergleiche haben keine Ergebnistextvariable, EQ, CMP
       ldstr.LoadString(IDS_STRING73);
       error(ldstr);
     }
     if ((pBefehl->Store == 0) && (pBefehl->Val1 < 3)) {
       // Zuweisungen haben eine Ergebnistextvariable
       ldstr.LoadString(IDS_STRING73);
       error(ldstr);
     }
     // Textvariablenreferenz
     if (pBefehl->Val2 != 0)
       storeRef('t', pBefehl->Val2, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if ((pBefehl->Val1 != 0) && (pBefehl->Val1 != 2) && (pBefehl->Val3 != 0))
       storeRef('t', pBefehl->Val3, relBlocknummer, Schrittnummer, Pruefschrittnummer);
     if (pBefehl->Store != 0)
       storeRef('T', pBefehl->Store, relBlocknummer, Schrittnummer, Pruefschrittnummer);

     // Kommando und Operanden verdichten
     pBefehl->Val1 = (pBefehl->Val1 & 0x0f) | ((pBefehl->Val2 & 0x3f) << 4) | ((pBefehl->Val3 & 0x3f) << 10);

     if ((pBefehl->Val1 & 0x000f) != 2) {
       // Text einsortieren, außer bei MID
       leng = 12 * sizeof(float);
       strAnzeigetext = getTPar(strAnzeigetext, 64);  // Länge TX-Variable
       if ((variante & VARIANTE_OPTION_02) &&
         strAnzeigetext.GetLength() > leng &&
         strAnzeigetext.FindOneOf(_T("abcdefghijklmnopqrstuvwxyz{|}~\\\x60\x7F")) == -1
         ) {
         int    i;
         char *ps, *pd;
         // mit Kompression
         // a-z {|}~\ 0x60 und 0x7F
         memset(asciiAnzeigetext, 0, sizeof(asciiAnzeigetext));
         if (CUniConversion::str_to_ascii(
           strAnzeigetext,    //LPCSTR lpszSrc, 
           asciiAnzeigetext,  //LPTSTR lpszDst, Länge unverändert
           65         //DWORD cchDstLength + Endezeichen 0x00
         ) < 0) {
           ldstr.LoadString(IDS_STRING71);
           error(ldstr);
           break;
         }

         for (i = 0, ps = asciiAnzeigetext; i < 64; i++, ps++) {
           if (*ps >= 0x20) *ps -= 0x20;  // 0x20-0x5f -> 0x00-0x3f
           else *ps = 0x5C - 0x20;    // 0x00-0x1f -> 0x3C      Ctrl-Zeichen-Kodierung
         }
         for (i = 0, ps = pd = asciiAnzeigetext; i < 64 / 4; i++, ps += 4, pd += 3) {
           pd[0] = (ps[0] & 0x3F) | ((ps[3] & 0x30) << 2);
           pd[1] = (ps[1] & 0x3F) | ((ps[3] & 0x0C) << 4);
           pd[2] = (ps[2] & 0x3F) | ((ps[3] & 0x03) << 6);
         }
         memcpy((char*)&pBefehl->Val2, asciiAnzeigetext, leng);
         pBefehl->B2 = 1;
       }
       else {
         // ohne Kompression, entspricht alter Variante
         if (strAnzeigetext.GetLength() > leng) {
           ldstr.LoadString(IDS_STRING71);
           tmp.Format(_T(" (%d)"), strAnzeigetext.GetLength());
           error(ldstr + tmp);
           break;
         }
         pBefehl->B2 = 0;
         if (CUniConversion::str_to_ascii(
           strAnzeigetext,    //LPCSTR lpszSrc, 
           asciiAnzeigetext,  //LPTSTR lpszDst, Länge unverändert
           leng + 1       //DWORD cchDstLength + Endezeichen 0x00
         ) < 0) {
           ldstr.LoadString(IDS_STRING71);
           error(ldstr);
           break;
         }
         else memcpy((char*)&pBefehl->Val2, asciiAnzeigetext, leng);  // Länge ohne Endezeichen 0x00
       }

       /*
           // alte Textkonvertierung
           leng = 12 * sizeof(float);
           strAnzeigetext = getTPar( strAnzeigetext, leng);
           // Win - > Dos Zeichenkonvertierung und move i
           if( CUniConversion::str_to_ascii(
                     strAnzeigetext,    //LPCSTR lpszSrc,
                     asciiAnzeigetext,  //LPTSTR lpszDst, Länge unverändert
                     leng+1       //DWORD cchDstLength + Endezeichen 0x00
                     ) < 0 ) {
             ldstr.LoadString(IDS_STRING71);
             error(ldstr);
           } else memcpy((char*)&pBefehl->Val2, asciiAnzeigetext, leng);  // Länge ohne Endezeichen 0x00
       */
     }
     // Maske
     if (pBefehl->Val1 < 3) {
       // Nur Vergleiche benötigen die Maske
       break;
     }
     if (strHelp7 == "\"\"") {
       pBefehl->B1 = 0;
       break;
     }
     if ((strHelp7.GetLength() == 3) && (strHelp7[0] == '\"') && (strHelp7[2] == '\"')) {
       _TCHAR stxt[2];
       char dtxt[2];
       stxt[0] = strHelp7[1]; stxt[1] = 0;
       // Win - > Dos Zeichenkonvertierung und move in Var1 bis Var7
       CharToOemBuff(stxt,  //LPCSTR lpszSrc, 
         dtxt,  //LPTSTR lpszDst, Länge unverändert
         1    //DWORD cchDstLength 
       );
       if ((dtxt[0] >= 0x20) && (dtxt[0] <= 0x7F)) {
         pBefehl->B1 = (unsigned char)dtxt[0];
         break;
       }
     }
     ldstr.LoadString(IDS_STRING82);
     error(ldstr);
     break;
   }
 }

 // PTB - Umrechnung
 if( m_rPTB != 1.0f ) {
   switch(blockType) {
    case bVorgabe:
      konvertTime(pBefehl->Var2, true);
      konvertTime(pBefehl->Var4, true);
      konvertTime(pBefehl->Var6, true);
      konvertTime(pBefehl->Var8, true);
      konvertTime(pBefehl->Var9);
      break;
    case bToleranz:
    case bToleranz_Differenz:
    case bStabilisierung:
    case bMaximum:
    case bMinimum:
    case bArithmetikblock_Statistik:
    case bFilter_Messsignal:
    case bSchwellzeit:
      konvertTime(pBefehl->Var3);
      konvertTime(pBefehl->Var4);
      break;
    case bLeistungspruefung:
    case bLeistungspruefung_Differenz:
      konvertTime(pBefehl->Var2);
      konvertTime(pBefehl->Var3);
      konvertTime(pBefehl->Var4);
      break;
    case bToleranzpruefung_Vergleich:
      konvertTime(pBefehl->Var3);
      konvertTime(pBefehl->Var4);
      konvertTime(pBefehl->Var7);
      break;
    case bDigitalwert:
    case bDigitalwert_Flanke:
      konvertTime(pBefehl->Var1);
      konvertTime(pBefehl->Var2);
      break;
    case bToleranzpruefung_Knickpunkt:
      konvertTime(pBefehl->Var3);
      konvertTime(pBefehl->Var4);
      konvertTime(pBefehl->Var6, true);
    case bHistorie:
      konvertTime(pBefehl->Var3);
      konvertTime(pBefehl->Var4);
      konvertTime(pBefehl->Var7);
      break;
    case bToleranzpruefung_Schwellzeit:
      konvertTime(pBefehl->Var3);
      konvertTime(pBefehl->Var4);
      konvertTime(pBefehl->Var6);
      break;
    case bArchivierung:
      konvertTime(pBefehl->Var3);
      break;
   }
 }

 if (!(variante & VARIANTE_INTERN_UMS))
 {
   // allgemeine Tests
   switch (blockType) {
   case bSchwellzeit:          // 2 Messkanäle testen
   case bToleranzpruefung_Schwellzeit:
   case bKorrekturblock_Vorgabesignal:
     // Meßkanal und Stellkanal testen
     if (!(0 < pBefehl->Val2)) {
       ldstr.LoadString(IDS_STRING15);
       error(ldstr);
     }
   case bToleranz:
   case bSchnelle_Toleranz:
   case bToleranz_Differenz:
   case bLeistungspruefung:
   case bSchnelle_Leistungspruefung:
   case bLeistungspruefung_Differenz:
   case bToleranzpruefung_Vergleich:
   case bStabilisierung:
   case bKorrekturblock_Messsignal:
   case bToleranzpruefung_Knickpunkt:
   case bMaximum:
   case bMinimum:
   case bHistorie:
   case bDigitalwert:
   case bDigitalwert_Flanke:
   case bFilter_Messsignal:
     // Messkanal darf nicht Null sein
     if (!(0 < pBefehl->Val1)) {
       ldstr.LoadString(IDS_STRING16);
       error(ldstr);
     }
     break;
   case bAbfrage:
     // Messkanal darf nicht Null sein
     if (!(0 < pBefehl->Val3)) {
       ldstr.LoadString(IDS_STRING16);
       error(ldstr);
     }
     break;
   }
 }
 // Bedingungen nach Lastenheft prüfen, wenn eine Parameterliste existiert.
 if(CounterPar > 0 && !(variante & VARIANTE_INTERN_UMS) ) {
 switch(blockType)  {
    case bMaximum:  // Schwellenwert <= 0
              // V6 <= 0
                     if(isVal(pBefehl->Var6)) {
                 if(!(0 >= pBefehl->Var6)) {
                   ldstr.LoadString(IDS_STRING17);
                   error(ldstr);
                }
                     }
              goto Test1;

    case bMinimum:   // Schwellenwert >= 0
              // V6 >= 0
                     if(isVal(pBefehl->Var6)) {
                if(!(0 <= pBefehl->Var6)) {
                  ldstr.LoadString(IDS_STRING18);
                  error(ldstr);
                }
                     }
Test1: 
    case bToleranz:         
    case bToleranz_Differenz:
              // KW < GW; 0 <= T1 <= T2
              // V1 < V2; 0 <= V3 <= V4
                  if(isVal(pBefehl->Var1) && isVal(pBefehl->Var2)) {
                              if(!(pBefehl->Var1 < pBefehl->Var2))
                                error(ldkg);
                            }
                            if(isVal(pBefehl->Var3) && isVal(pBefehl->Var4)) {
                              if(!(0 <= pBefehl->Var3))
                                error(_T("0 <= T1\n") );
                              if(!(pBefehl->Var3 <= pBefehl->Var4))
                                error(_T("T1 <= T2\n") );
                            }
         break;
    case bSchnelle_Toleranz:         
              // KW < GW; -9 <= T1 <= T2
              // V1 < V2; -9 <= V3 <= V4
                  if(isVal(pBefehl->Var1) && isVal(pBefehl->Var2)) {
                              if(!(pBefehl->Var1 < pBefehl->Var2))
                                error(ldkg);
                            }
                            if(isVal(pBefehl->Var3) && isVal(pBefehl->Var4)) {
                              if(!(-9 <= pBefehl->Var3))
                                error(_T("-9 <= T1\n") );
                              if(!(pBefehl->Var3 <= pBefehl->Var4))
                                error(_T("T1 <= T2\n") );
                            }
         break;
    case bToleranzpruefung_Knickpunkt:
              // KW < GW; 0 < T1 < T2
              // V1 < V2; 0 < V3 < V4
              if(isVal(pBefehl->Var1) && isVal(pBefehl->Var2)) {
                              if(!(pBefehl->Var1 < pBefehl->Var2))
                                error(ldkg);
                            }
                            if(isVal(pBefehl->Var3) && isVal(pBefehl->Var4)) {
                              if(!(0 < pBefehl->Var3))
                                error(_T("0 < T1\n") );
                              if(!(pBefehl->Var3 < pBefehl->Var4))
                                error(_T("T1 < T2\n") );
                            }
         break;
    case bHistorie:         // Test ist versionsabhängig
                            // Alle außer 13.* und 14.* (Berliner Versionen)
                            // Kennung ist VARIANTE_HEAD, diese wird nur in Berlin benutzt
                            // KW < GW; 0 <= T1 < T2   T2 - T1 <= 20s
              // V1 < V2; 0 <= V3 < V4   V4 - V3 <= 20000 / 100000
              if(isVal(pBefehl->Var1) && isVal(pBefehl->Var2)) {
                              if(!(pBefehl->Var1 < pBefehl->Var2))
                                error(ldkg);
                            }
                            if(isVal(pBefehl->Var3) && isVal(pBefehl->Var4)) {
                              if(!(0.0 <= pBefehl->Var3))
                                error(_T("0 <= T1\n") );
                              if(!(pBefehl->Var3 < pBefehl->Var4))
                                error(_T("T1 < T2\n") );
                              if( variante & VARIANTE_HEAD ) {
                                  if(! ((pBefehl->Var4 - pBefehl->Var3) <= 200000.01) )
                                    error(_T("T2 - T1 <= 200s\n") );
                              } else {
                                  if(! ((pBefehl->Var4 - pBefehl->Var3) <=  20000.01) )
                                    error(_T("T2 - T1 <= 20s\n") );             
                              }
                            }
         break;
    case bLeistungspruefung:
    case bLeistungspruefung_Differenz: 
    case bSchnelle_Leistungspruefung:
              // 0 <= T1 <= T2 <= T3
              // 0 <= V2 <= V3 <= V4
                            if(isVal(pBefehl->Var2) && isVal(pBefehl->Var3) && isVal(pBefehl->Var4)) {
                              if(!(0 <= pBefehl->Var2))
                                error(_T("0 <= T1\n") );
                              if(!(pBefehl->Var2 <= pBefehl->Var3))
                                error(_T("T1 <= T2\n") );
                              if(!(pBefehl->Var3 <= pBefehl->Var4))
                                error(_T("T2 <= T3\n") );
                            }
         break;
    case bDigitalwert:      
    case bDigitalwert_Flanke:
              // 0 <= T1 <= T2
              // 0 <= V1 <= V2
                            if(isVal(pBefehl->Var1) && isVal(pBefehl->Var2)) {
                              if(!(0 <= pBefehl->Var1))
                                error(_T("0 <= T1\n") );
                              if(!(pBefehl->Var1 <= pBefehl->Var2))
                                error(_T("T1 <= T2\n") );
                            }
              // Kanal > 0
              // Val1  > 0
                            if(!(0 < pBefehl->Val1)) {
                ldstr.LoadString(IDS_STRING20);
                                error(ldstr);
                            }
         break;
    case bStabilisierung:   // 0 < Sollwert; 0 < T1; 10 <= Tmw; 0 < Anzahl; 0 < N    
              // 0 < V1      ; 0 < V2; 10 <= V3 ; 0 < V4    ; 0 < Val2 
                            if(isVal(pBefehl->Var1)) {
                if(!(0 < pBefehl->Var1)) {
                  ldstr.LoadString(IDS_STRING21);
                  error(ldstr);
                }
                            }
                            if(isVal(pBefehl->Var2)) {
                              if(!(0 < pBefehl->Var2))
                                error(_T("0 < T1\n") );
                            }
                            if(isVal(pBefehl->Var3)) {
                              if(!(10 <= pBefehl->Var3))
                                error(_T("10 <= Tmw\n") );
                            }
                            if(isVal(pBefehl->Var4)) {
                if(!(0 < pBefehl->Var4)) {
                  ldstr.LoadString(IDS_STRING22);
                  error(ldstr);
                }
                            }
                            if(!(0 < pBefehl->Val2))
                              error(_T("0 < N\n") );
         break;
    case bArithmetikblock_Verknuepfungen:
              // KW < GW => V3 < V4
                            if(isVal(pBefehl->Var3) && isVal(pBefehl->Var4)) {
                              if(!(pBefehl->Var3 < pBefehl->Var4))
                                error(ldkg);
                            }
         break;
  case bArithmetikblock_Statistik:
              // 0 <= T1 < T2
              // 0 <= V3 < V4 
                            if(isVal(pBefehl->Var3) && isVal(pBefehl->Var4)) {
                              if(!(0 <= pBefehl->Var3))
                                error(_T("0 <= T1\n") );
                              if(!(pBefehl->Var3 < pBefehl->Var4))
                                error(_T("T1 < T2\n") );
                            }
     break;
    case bArithmetikblock_Logik:
              // KW <= GW => V3 <= V4
                            if(isVal(pBefehl->Var3) && isVal(pBefehl->Var4)) {
                              if(!(pBefehl->Var3 <= pBefehl->Var4))
                                error(ldkg);
                            }
         break;
    case bToleranzpruefung_Vergleich:
              //  KW < GW;         V1 <  V2 
              //  0  <= T1;        0  <= V3
              // 10  <=Tmw;       10  <= V4
              //  0  < Anz;        0  <  V6
                            // Mode1:
                            // T2 < T1 + Tmv;   V7 <  V3 + V4
              if(isVal(pBefehl->Var1) && isVal(pBefehl->Var2)) {
                if(!(pBefehl->Var1 < pBefehl->Var2))
              error(ldkg);
              }
              if(isVal(pBefehl->Var3)) {
                if(!(0 <= pBefehl->Var3))
              error(_T("0 <= T1\n") );
              }
              if(isVal(pBefehl->Var4)) {
                if(!(10 <= pBefehl->Var4))
              error(_T("10 <= Tmw\n") );
              }
              if(isVal(pBefehl->Var6)) {
                if(!(0 < pBefehl->Var6)) {
                  ldstr.LoadString(IDS_STRING22);
                  error(ldstr);
                }
              }
                            if( pBefehl->B1 ) {
                  if( isVal(pBefehl->Var3) && isVal(pBefehl->Var4) && isVal(pBefehl->Var7) ) {
                    if(!( pBefehl->Var3 + pBefehl->Var4 <= pBefehl->Var7))
                      error(_T("T1+Tmv <= T2\n") );
                  }
                            }
     break;
  case bKorrekturblock_Messsignal:
              // KW < GW   ; V1 <  V2 
              // 0 < Anzahl; 0 < Val2 
              if(isVal(pBefehl->Var1) && isVal(pBefehl->Var2)) {
                if(!(pBefehl->Var1 < pBefehl->Var2))
              error(ldkg);
              }
                            if(!(0 < pBefehl->Val2)) {
                ldstr.LoadString(IDS_STRING22);
                error(ldstr);
              }
     break;
  case bKorrekturblock_Vorgabesignal:
              // KW < GW; V2 <  V3 
              // 0 < Anzahl; 0 < Val3
              if(isVal(pBefehl->Var2) && isVal(pBefehl->Var3)) {
                if(!(pBefehl->Var2 < pBefehl->Var3))
              error(ldstr);
              }
                            if(!(0 < pBefehl->Val3)) {
                ldstr.LoadString(IDS_STRING22);
                error(ldstr);
              }
         break;
  case bAnzeige:
              if( pBefehl->B2 || pBefehl->B3 ) {
                // Zahlen
                // KW < KWgut < GWgut < GW
                // V1 < V3    < V4    < V2
                if(isVal(pBefehl->Var1) && isVal(pBefehl->Var2) && isVal(pBefehl->Var3) && isVal(pBefehl->Var4)) {
                  if(!(pBefehl->Var1 < pBefehl->Var3)) {
                  ldstr.LoadString(IDS_STRING26);
                  error(ldstr);
                  }
                  if(!(pBefehl->Var3 < pBefehl->Var4)) {
                  ldstr.LoadString(IDS_STRING24);
                  error(ldstr);
                  }
                  if(!(pBefehl->Var4 < pBefehl->Var2)) {
                  ldstr.LoadString(IDS_STRING25);
                  error(ldstr);
                  }
                }
              }
     break;     
    case bSchwellzeit:  
              if( !pBefehl->B3 ) {
                // Zahlen
                // T  > Ts
                // V3 > V4 
                if(isVal(pBefehl->Var3) && isVal(pBefehl->Var4)) {
                  if(!(pBefehl->Var3 > pBefehl->Var4)) {
                  error(_T("SSZ <= T\n") );                 
                  }
                }
              }
     break;     
    case bToleranzpruefung_Schwellzeit:
              // KW < GW;
              // V1 < V2;
                  if(isVal(pBefehl->Var1) && isVal(pBefehl->Var2)) {
                              if(!(pBefehl->Var1 < pBefehl->Var2))
                                error(ldkg);
                            }
              // 0 <= T1 <= T2 <= T3
              // 0 <= V3 <= V4 <= V6
                            if(isVal(pBefehl->Var3) && isVal(pBefehl->Var4) && isVal(pBefehl->Var6)) {
                              if(!(0 <= pBefehl->Var3))
                                error(_T("0 <= T1\n") );
                              if(!(pBefehl->Var3 <= pBefehl->Var4))
                                error(_T("T1 <= T2\n") );
                              if(!(pBefehl->Var4 <= pBefehl->Var6))
                                error(_T("T2 <= T3\n") );
                            }
    break;
    case bArchivierung:   // Spaltennummer 1 - 6
              if(!(1 <= pBefehl->Val1) || !(pBefehl->Val1 <= 6) ) 
                error(_T("Index 1 - 6\n") );
              // Spaltennummer <= Spaltenanzahl
              if(!(pBefehl->Val1 <= pBefehl->Val3) ) 
                error(_T("Spaltenanzahl zu klein\n") );
              // Keine Zeitüberwachung, da Tastzeit variabel
/*
              // T1 < 6500, 10000, 20000ms
              switch( pBefehl->Val3 ) {
              case 1: 
              case 2: t = 20000.01f; break;
              case 3:
              case 4: t = 10000.01f; break;
              case 5:
              case 6: t =  6500.01f; break;
              }
                            if( isVal(pBefehl->Var3) &&   !(pBefehl->Var3 <= t) )
                error("T1 <= 20/10/6,5s\n");
*/
         break;
    case bArchivauswertung: // Spaltennummer Suchwert 1 - 6
              if(!(1 <= pBefehl->Val1) || !(pBefehl->Val1 <= 6) ) {
                tmp.LoadString(IDS_STRING100);
                error( tmp );
              }
              // Spaltennummer Bezugswert 1 - 6
              if(!(1 <= pBefehl->Val2) || !(pBefehl->Val2 <= 6) ) {
                tmp.LoadString(IDS_STRING100);
                error( tmp );
              }
              // Spaltennummer <= Spaltenanzahl
              if(!(pBefehl->Val2 <= pBefehl->Val3) ) {
                tmp.LoadString(IDS_STRING101);
                error( tmp );
              }
              if(!(pBefehl->Val1 <= pBefehl->Val3) ) {
                tmp.LoadString(IDS_STRING101);
                error( tmp );
              }
         break;
  case bTextverarbeitung: // Start <= Stop; MID
              if( ((pBefehl->Val1 & 0x000f) == 2) && (pBefehl->Var1 > pBefehl->Var2) ) {
                tmp.LoadString(IDS_STRING102);
                error( tmp );
              }
    break;
   }
 }

 // Befehlszähler weiterschalten
 pBefehl++;
 CounterBefehl++;
 if(CounterBefehl >= maxBefehle - 2) {
   ldstr.LoadString(IDS_STRING27);
   error(ldstr);
   return 0;
 }
 return  1;
}
/*END_WIZARD_FELD*/

// Schlußsequenz anfügen
int CKonvert::append(void)
{
 // bVorgabe
 memset(pBefehl, 0, sizeof(TBefehl));
 pBefehl->Modul  = bVorgabe;
 pBefehl->Intern = CounterSchritte+1;
 pBefehl->Store  = 1;
 pBefehl->Link   = 1;
 pBefehl++;
 CounterBefehl++;
 // bError
 memset(pBefehl, 0, sizeof(TBefehl));
 pBefehl->Link  = 1;
 pBefehl++;
 CounterBefehl++;
 return fehler == 0;
}

// Label durch Schrittnummern ersetzen
int CKonvert::ReplaceLabel(CString& prg)
{
    CString     str, label;
    int         pos, pos1, flag;

    flag = 0;
    while( (pos = prg.Find(_T("goto"))) != -1 ) {
        str += prg.Left(pos);
        prg  = prg.Mid(pos);
        // pos hinter "goto"
        pos  = prg.Find(':');
        pos1 = prg.Find(';');
        if( (pos == -1) || (pos1 == -1) || (pos > pos1) ) {
            str += prg;
            prg = str;
            return 0;
        }
        str += prg.Left(pos);
        prg  = prg.Mid(pos+1);
        // pos hinter ":"
        // : muss vor ; stehen
        if( (pos < pos1) && _istalpha(prg[0]) ) {
            pos = 0;
            while( isalnum(prg[pos]) ) pos++;
            label = CString(':') + prg.Left(pos) + CString('#');
            label.MakeLower();
            if( (pos1=LabelStr.Find( label ))!= -1) {
                label = LabelStr.Mid(pos1 + label.GetLength(), 4);
                str  += label.SpanExcluding(_T(":") );
                prg   = prg.Mid(pos);
            }
        } else 
            str += CString(':');
    }
    str += prg;

    //  Neuen String zurückgeben
    prg = str;

    return 0;
}

int CKonvert::readTmpLstFile(LPCTSTR lstname, int snr)
{
    CUtf8File     flst;
    CString         str, name;
    int             pos;
    unsigned short  line;
    unsigned short  addr;
    LPTSTR          pe;

    // Adressreferenzen lesen und konvertieren
    if( !flst.Open(lstname, CFile::modeRead, TRUE) ) {
        return 0;
    }
    while( flst.ReadString( str ) ) {
        if( str[0] == '>' ) {// Referenzzeile
            if( (pos=str.ReverseFind('[')) == -1 )
                break;
            str  = str.Mid(pos+1);
            line = (unsigned short)_tcstoul(str,  &pe, 10);
            addr = (unsigned short)_tcstoul(pe+1, &pe, 10);
            BlockLineAddr.Add(snr);
            BlockLineAddr.Add(line);
            BlockLineAddr.Add(addr);
        }
    }
    flst.Close();
    if( !DeleteFile(lstname) ) {
        return 0;
    }

    return 1;
}

// Label durch Schrittnummern ersetzen
int CKonvert::KonvertLabel(LPCTSTR p)
{
    CString     str, label;
    int         pos;

    label = CString(':') + CString(p) + CString('#');
    label.MakeLower();
    if( (pos=LabelStr.Find( label ))!= -1) {
        return _tstoi( LabelStr.Mid(pos + label.GetLength(), 4));
    }

    return -1;
}

CString CKonvert::getLabel(int sn)
{
    CString     str;
    int         pos;

  str.Format(_T("#%d:"), getSchritt(sn));
  if( (pos=LabelStr.Find( str ))!= -1 ) {
        str = LabelStr.Left(pos);
        pos = str.ReverseFind(':');
        str = str.Mid(pos);
        return str;
    }

    return CString(""); 
}

int CKonvert::check(LPCTSTR resvar, bool DeepTest) // DeepTest nicht für UMS
{
 const int    maxbuf = 256;
 TBefehl    *pb, *pvb;
 int            i, j, ok, pos;
 int            cc, ccgf;
 int      archiv1ba, archiv1, archiv2ba, archiv2, abfragen, bit, spalten;
 unsigned int   bn, sn;
 float      *pf;
 _TCHAR         buf[maxbuf], *pe;
 int      layout_ver, konvert_ver_lo;
 CString    str, tmp;
 ParRef     *ptPR;
 // Layout-Version kompatibel zu Konverter-Version ?
 pos      = CString(KONVERTER_VERSIONS_STR).Find('.');  // Punkt begrenzt highword
 str      = CString(KONVERTER_VERSIONS_STR).Mid(pos+1);
 konvert_ver_lo = _tcstoul(str                  , &pe, 10);
 str      = CString(',') + CString(KONVERTER_VERSIONS_STR).Left(pos) + CString(',');

 if (DeepTest)
 {
   // Mindestens 1 Endblock ?
   // Schleife vor angefügten Blöcken beenden
   for (pb = pbPC, ok = 0; nextBlk(pb)->Modul != bError; pb += pb->Link)
     if (pb->Modul == bEnde) {
       ok = 1;
       // Layout-Versionsword aus Val1 lesen
       layout_ver = pb->Val1 >> 8;
       tmp.Format(_T(",%2d,"), layout_ver);
       if (str.Find(tmp) == -1) {
         ldstr.LoadString(IDS_STRING49);
         //_stprintf_s(buf, maxbuf, _T("%s: %d.%d  -  %s"), ldstr, pb->Val1 >> 8, pb->Val1 & 0x00ff, KONVERTER_VERSIONS_STR);
         // obiger Ergebnisstring war zu lang
         _stprintf_s(buf, maxbuf, _T("%s: %d.%d"), ldstr.GetBuffer(), pb->Val1 >> 8, pb->Val1 & 0x00ff);
         semerr(0, 0, buf);
       }
       pb->Val2 = 0;
       pb->Val3 = 0;

       pb->Var1 =
         pb->Var2 =
         pb->Var3 =
         pb->Var4 =
         pb->Var5 =
         pb->Var6 =
         pb->Var7 =
         pb->Var8 =
         pb->Var9 = 0.0;


     }
   if (!ok) {
     ldstr.LoadString(IDS_STRING28);
     semerr(0, 0, ldstr);
   }
   // Not-Aus-Block ?
   for (pb = pbPC, ok = 0; nextBlk(pb)->Modul != bError; pb += pb->Link)
     if (pb->Modul == bProgrammabbruch)
       ok++;
   if (ok != 1) {
     ldstr.LoadString(IDS_STRING29);
     semerr(0, 0, ldstr);
   }
   // Zähler der Historien-, Archivierungs-, Archivauswertungs. und Scriptblöcke pro Schritt
   cc = archiv1 = archiv2 = archiv1ba = archiv2ba = spalten = 0;
   for (pvb = pb = pbPC, bn = 1; nextBlk(pb)->Modul != bError; bn += pb->Link, pb += pb->Link) {
     relBlocknummer = relBlocknr[pb - pbPC];
     Pruefschrittnummer = Pruefschrittnr[pb - pbPC];
     switch (pb->Modul) {
     case bVorgabe:     if (cc > maxHistorie) {
       ldstr.LoadString(IDS_STRING30);
       semerr(bn, pb->Intern - 1, ldstr);
     }
                if ((archiv1 > 6) || (archiv2 > 6) || (archiv1 * archiv2 > 0) || (cc && (archiv1 || archiv2))) {
                  ldstr.LoadString(IDS_STRING67);
                  semerr(bn, pb->Intern - 1, ldstr);
                }
                pvb = pb;     // aktuellen Vorgabeblock merken
                cc = archiv1 = archiv2 = archiv1ba = archiv2ba = spalten = 0;
                break;
     case bHistorie:      pb->Val2 = cc++;
       break;
     case bArchivierung:    // Speicherwertspalte
                 // Blöcke zählen
       archiv1++;
       // Spaltennummer merken
       bit = 1 << pb->Val1;
       if (archiv1ba & bit) {
         ldstr.LoadString(IDS_STRING68);
         _stprintf_s(buf, maxbuf, _T("%s%d\n"), ldstr.GetBuffer(), pb->Val2);
         semerr(bn, pvb->Val3, buf);
       }
       archiv1ba |= bit;
       // Spaltenanzahl gleich?
       if (spalten == 0) spalten = pb->Val3;
       else if (spalten != pb->Val3) {
         ldstr.LoadString(IDS_STRING69);
         semerr(bn, pvb->Val3, ldstr);
       }
       break;
     case bArchivauswertung: // Suchwertspalte
                 // gleiche Spaltennummer erwünscht
                 // Blöcke zählen
       archiv2++;
       // Bezugswertspalte; Dieser Fall ist möglich!
       /*
       bit = 1 << pb->Val2;
       if( archiv2ba & bit ) {
         ldstr.LoadString(IDS_STRING68);
         _stprintf_s(buf, maxbuf, "%s%d\n", ldstr, pb->Val2);
         semerr(bn, pvb->Val3, buf);
       }
       archiv2ba |= bit;
       */
       // Spaltenanzahl gleich?
       if (spalten == 0) spalten = pb->Val3;
       else if (spalten != pb->Val3) {
         ldstr.LoadString(IDS_STRING69);
         semerr(bn, pvb->Val3, ldstr);
       }
       break;
     case bScript:      cc += maxHistorie;   // Nur ein Script je Schritt sinnvoll
       break;
     }
   }

   // Zähler der Abfrageblöcke pro Schritt
   for (pvb = pb = pbPC, bn = 1, abfragen = 0; nextBlk(pb)->Modul != bError; bn += pb->Link, pb += pb->Link) {
     relBlocknummer = relBlocknr[pb - pbPC];
     Pruefschrittnummer = Pruefschrittnr[pb - pbPC];
     switch (pb->Modul) {
     case bVorgabe:     // Letzten Schritt bearbeiten
       if (abfragen) {
         for (TBefehl *pbls = pvb; pbls[1].Modul != bError && pbls < pb; pbls++) {
           if (pbls->Modul == bAbfrage ||
             pbls->Modul == bAbfrageValue ||
             pbls->Modul == bAbfrageDlg ||
             pbls->Modul == bAbfrageSignal
             )
             pbls->Val2 = abfragen;
         }
       }
       pvb = pb;      // aktuellen Vorgabeblock merken
       abfragen = 0;
       break;
     case bAbfrage:
     case bAbfrageValue:
     case bAbfrageDlg:
     case bAbfrageSignal: abfragen++;
       break;
     }
   }

   // Script-Puffer leer ?
    // Puffer leer ?
   tmp = Extender.check();
   if (!tmp.IsEmpty()) error(tmp);

   // Alle Sprungziele vorhanden
   // Sprungrichtung ?
   // Sprungziele konvertieren
   // Wiederholungen markieren
   for (pb = pbPC, bn = 1; nextBlk(pb)->Modul != bError; bn += pb->Link, pb += pb->Link) {
     ok = 1;
     relBlocknummer = relBlocknr[pb - pbPC];
     Pruefschrittnummer = Pruefschrittnr[pb - pbPC];
     switch (pb->Modul) {
     case bVorgabe:     sn = pb->Intern;
       break;

     case bVerzweigung_GUT_FEHL:
       ok = 1;
       if ((pb->Val2 == 0) || (pb->Val2 > CounterSchritte)) {
         ldstr.LoadString(IDS_STRING31);
         semerr(bn, sn, ldstr);
         ok = 0;
       }
       if ((pb->Val1 == 0) || (pb->Val1 > CounterSchritte)) {
         ldstr.LoadString(IDS_STRING32);
         semerr(bn, sn, ldstr);
         ok = 0;
       }
       // Konvertierung
       if (ok) {
         pb->Val1 = pSchritte[pb->Val1];
         pb->Val2 = pSchritte[pb->Val2];
       }
       break;

     case bVerzweigung_Pruefnummer:
       if (pb->Val1 <= sn) {
         ldstr.LoadString(IDS_STRING33);
         semerr(bn, sn, ldstr);
         break;
       }
       if (pb->Val1 > CounterSchritte) {
         ldstr.LoadString(IDS_STRING34);
         semerr(bn, sn, ldstr);
         break;
       }
       // Konvertierung
       pb->Val1 = pSchritte[pb->Val1];
       break;

     case bUnterprogrammruf:
       if (variante & VARIANTE_OPTION_02) {
         if (pb->Val1 == 0 && pb->Val2 == 0) {
           ldstr.LoadString(IDS_STRING34);
           semerr(bn, sn, ldstr);
           break;
         }
         if (pb->Val1 > CounterSchritte) {
           ldstr.LoadString(IDS_STRING34);
           semerr(bn, sn, ldstr);
           break;
         }
         pb->Val1 = pSchritte[pb->Val1];
         if (pb->Val2 > CounterSchritte) {
           ldstr.LoadString(IDS_STRING34);
           semerr(bn, sn, ldstr);
           break;
         }
         pb->Val2 = pSchritte[pb->Val2];
       }
       else {
         if ((pb->Val1 == 0) || (pb->Val1 > CounterSchritte)) {
           ldstr.LoadString(IDS_STRING34);
           semerr(bn, sn, ldstr);
           break;
         }
         pb->Val2 = pb->Val1 = pSchritte[pb->Val1];
       }
       break;

     case bProgrammabbruch:
       /* Auswertung im MoniFB
        (* NOT-Baustein ? *)
        IF Befehl.Modul = TBlock.bProgrammabbruch THEN
          IpState.NotPointer        := UDINT_TO_DINT(Befehl.Val1);
          DauerlaufStatus.RestartPointer  := UDINT_TO_DINT(Befehl.Val2);  (* RestartPointer *)
          DauerlaufStatus.SignalPointer := UDINT_TO_DINT(Befehl.Val3);  (* SignalPointer  *)
          DauerlaufStatus.MappingIndex  := UDINT_TO_DINT(Befehl.Val4);  (* MappingPointer *)
          umlaufFirstChannel        := REAL_TO_INT(Befehl.Var6);  (* erster Kanal, der überschrieben wird *)
          umlaufFirstVaraiable        := REAL_TO_INT(Befehl.Var7);  (* erster Variable, die übertragen wird *)
          umlaufCopyData              := REAL_TO_INT(Befehl.Var8);  (* Anzahl der Variablen, die übertragen werden *)
          Zeitfaktor            := REAL_TO_INT(Befehl.Var9);  (* Teiler für Aufzeichnung,  nur alle n Schritte wird gespeichert *)
        END_IF;
       */
       if ((pb->Val1 == 0) || (pb->Val1 > CounterSchritte)) {
         ldstr.LoadString(IDS_STRING34);
         semerr(bn, sn, ldstr);
         break;
       }
       pb->Val1 = pSchritte[pb->Val1];

       if ((pb->Val2 == 0) || (pb->Val2 > CounterSchritte)) {
         ldstr.LoadString(IDS_STRING34);
         semerr(bn, sn, ldstr);
         break;
       }
       pb->Val2 = pSchritte[pb->Val2];
       if ((pb->Val3 == 0) || (pb->Val3 > CounterSchritte)) {
         ldstr.LoadString(IDS_STRING34);
         semerr(bn, sn, ldstr);
         break;
       }
       pb->Val3 = pSchritte[pb->Val3];
       // Zeitbasis in Bruchteilen von 1ms
       pb->Var1 = m_rPTB != 1.0f ? m_rPTB : 0;
       break;

     case bWiederholfunktion:
       if (pb->Val1 <= sn) {
         ldstr.LoadString(IDS_STRING33);
         semerr(bn, sn, ldstr);
         break;
       }
       if (pb->Val1 > CounterSchritte) {
         ldstr.LoadString(IDS_STRING34);
         semerr(bn, sn, ldstr);
         break;
       }
       // Konvertierung für NextFlag setzen
       pbPC[pSchritte[pb->Val1]].Err = 1;
       // Endeschritt retten
       pb->Val3 = pb->Val1;
       // Konvertierung des Schleifenanfangs
       pb->Val1 = pSchritte[sn + 1];
       break;
       /*
         case bParametrierblock_Messkreis:
         case bParametrierblock_Regelkreis:
         case bParametrierblock_Zeitgeber:
                  if( variante & VARIANTE_START ) {
                    if((pb->Val2 != 0) && (pb->Val2 <= CounterSchritte)) {
                      // Konvertierung
                      pb->Val2 = pSchritte[pb->Val2];
                    }
                    // else abgeschalten bzw. Signal
                  }
                  break;
       */
     }
   }

   // Anzahl der Steuermodule testen
   // Steuerblöcke müssen am Schrittende stehen !!! Interpreterfehler seit erstem Projekt!!! 
   cc = ccgf = 0;
   for (pb = pbPC, bn = 1; nextBlk(pb)->Modul != bError; bn += pb->Link, pb += pb->Link) {
     relBlocknummer = relBlocknr[pb - pbPC];
     Pruefschrittnummer = Pruefschrittnr[pb - pbPC];
     switch (pb->Modul) {
     case bVorgabe:     if (cc + ccgf > 1) {
       ldstr.LoadString(IDS_STRING35);
       semerr(bn, sn, ldstr);
     }
                sn = pb->Intern;
                cc = pb->Err; // 1 = Next entspricht auch Steuerblock
                ccgf = 0;
                break;
     case bEnde:
     case bVerzweigung_Pruefnummer:
     case bWiederholfunktion:
     case bUnterprogrammruf:
       cc++;
       if ((nextBlk(pb)->Modul != bVorgabe) && (nextBlk(pb)->Modul != bError)) {
         ldstr.LoadString(IDS_STRING66);
         semerr(bn, sn, ldstr);
       }
       break;
     case bVerzweigung_GUT_FEHL:
       // Bei Multijump zählen alle Verzweigungsblöcke als einer
       if (variante & VARIANTE_MULTIJUMP)
         ccgf = 1;
       else {
         ccgf++;
         if ((nextBlk(pb)->Modul != bVorgabe) && (nextBlk(pb)->Modul != bError)) {
           ldstr.LoadString(IDS_STRING66);
           semerr(bn, sn, ldstr);
         }
       }
       break;
     case bUnterprogrammruecksprung:
       if ((nextBlk(pb)->Modul != bVorgabe) && (nextBlk(pb)->Modul != bError)) {
         ldstr.LoadString(IDS_STRING66);
         semerr(bn, sn, ldstr);
       }
       break;
     }
   }
 } // DeepTest

 // Benutzte Variablen markieren; 
 // von Scripten Variable bereits gesetzt
 // Flag:
 //    0     = nie beschrieben,
 //  128     = beschrieben,
 //  129 ... = im Schritt mehrfach beschrieben
 for(pb = pbPC, bn = 1; pb->Modul != bError;  bn+= pb->Link, pb+= pb->Link) {
  relBlocknummer    = relBlocknr[pb - pbPC];
  Pruefschrittnummer  = Pruefschrittnr[pb - pbPC];
  if(pb->Modul == bVorgabe) sn = pb->Intern;
  Tblock modul = DeepTest ? pb->Modul : pb->Modul == bVorgabe ? bVorgabe : bToleranz;
  switch(modul) {
   case bVorgabe:
          // neuer Schritt
          for(i = 1; i <= maxVar; i++) {
            if( resvar ) {
              // Test gegen Ressourcenvariable
              if(pVar[i] > 0x80) {
                tmp.Format(_T("v%02d "), i);
                if( _tcsstr(resvar, tmp) != NULL ) {
                  ldstr.LoadString(IDS_STRING78);
                  _stprintf_s(buf, maxbuf, ldstr,i);
                  semerr(0, sn-1, buf);
                }
              }
            }
            // Test auf mehrfach beschrieben im letzten Schritt
            if(pVar[i] > 0x81) {
              ldstr.LoadString(IDS_STRING36);
              _stprintf_s(buf, maxbuf, ldstr,i);
              semerr(0, sn-1, buf);
            }
            // mehrfach beschrieben für neuen Schritt rücksetzen
            pVar[i] &= 0x80;
          }
          if(nextBlk(pb)->Modul == bError)
                     break;
    case bScript: for(ptPR = pParRef; ptPR < pPR; ptPR++) {
            // Nur Quellen 
            if((ptPR->t == 'V') && (ptPR->sn == (short)sn) && (ptPR->rbn == relBlocknummer) ) {
              pVar[ptPR->pn] |= 0x80; /* global: wurde beschrieben */
                    pVar[ptPR->pn] +=    1; /* lokal : wie oft    "      */
            }
          }
          break;
  case bArithmetikblock_Verknuepfungen:     
          /* Arithmetikblock darf mehrfach beschrieben werden.
             Nur global Variable markieren            */
          pVar[pb->Store] |= 0x80; /* global: wurde beschrieben */
//          pVar[pb->Store] +=    1; /* lokal : wie oft    "    */
  break;
    case bToleranz:         
    case bSchnelle_Toleranz:         
    case bToleranz_Differenz:     
    case bLeistungspruefung:         
    case bLeistungspruefung_Differenz:
    case bSchnelle_Leistungspruefung:
    case bToleranzpruefung_Vergleich:
  case bStabilisierung:
  case bKonstante:
  case bMinimum:
  case bMaximum:
  case bHistorie:
    case bToleranzpruefung_Knickpunkt:
    case bKonstantenfeld:
    case bKonstantenfeld_Erweiterung:
    case bArithmetikblock_Logik:
    case bFrontEnd1:
    case bFrontEnd2:
    case bFrontEnd3:
    case bFrontEnd4:
    case bFrontEnd5:
    case bFrontEnd6:
    case bFrontEnd7:
    case bFrontEnd8:
    case bSchwellzeit:
    case bToleranzpruefung_Schwellzeit:
                     pVar[pb->Store] |= 0x80; /* global: wurde beschrieben */
                     pVar[pb->Store] +=    1; /* lokal : wie oft    "      */
  break;
    case bVerzweigung_GUT_FEHL: 
                     pVar[pb->Store] |= 0x80; /* global: wurde beschrieben */
                     // Bei Multijump nicht erhöhen
                     if( variante & VARIANTE_MULTIJUMP ) {
                         if(pVar[pb->Store] == 0 ) 
                             pVar[pb->Store] = 1; /* lokal : wie oft    "      */
                     }
                     else
                         pVar[pb->Store] +=    1; /* lokal : wie oft    "      */
  break;
  case bProgrammabbruch:
        if( pb->Val4 == 0 ) break;
          /* Sonderfall mit 3 Ausgängen */
           pVar[int(pb->Val4  )] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Val4  )] +=    1; /* lokal : wie oft    "      */
           pVar[int(pb->Val4+1)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Val4+1)] +=    1; /* lokal : wie oft    "      */
           pVar[int(pb->Val4+2)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Val4+2)] +=    1; /* lokal : wie oft    "      */
  break;
  case bArithmetikblock_Statistik:
          /* Sonderfall mit 5 Ausgängen */
           pVar[int(pb->Var1)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var1)] +=    1; /* lokal : wie oft    "      */
           pVar[int(pb->Var2)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var2)] +=    1; /* lokal : wie oft    "      */
           pVar[int(pb->Var5)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var5)] +=    1; /* lokal : wie oft    "      */
           pVar[int(pb->Var6)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var6)] +=    1; /* lokal : wie oft    "      */
           pVar[int(pb->Var7)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var7)] +=    1; /* lokal : wie oft    "      */
  break;
  case bArchivauswertung:
          /* Sonderfall mit 3 Ausgängen */
           pVar[int(pb->Var6)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var6)] +=    1; /* lokal : wie oft    "      */
           pVar[int(pb->Var7)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var7)] +=    1; /* lokal : wie oft    "      */
                     pVar[pb->Store]   |= 0x80; /* global: wurde beschrieben */
                     pVar[pb->Store]   +=    1; /* lokal : wie oft    "      */
  break;
  case bAbfrageValue:
          if( !pb->B1 ) break;
          /* Sonderfall mit 1 Ausgängen */
          if(pb->Var1)
          {
           pVar[int(pb->Var1)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var1)] +=    1; /* lokal : wie oft    "      */
          }
          if(pb->Var2)
          {
           pVar[int(pb->Var2)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var2)] +=    1; /* lokal : wie oft    "      */
          }
          if(pb->Var3)
          {
           pVar[int(pb->Var3)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var3)] +=    1; /* lokal : wie oft    "      */
          }
          if(pb->Var4)
          {
           pVar[int(pb->Var4)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var4)] +=    1; /* lokal : wie oft    "      */
          }
          if(pb->Var5)
          {
           pVar[int(pb->Var5)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var5)] +=    1; /* lokal : wie oft    "      */
          }
          if(pb->Var6)
          {
           pVar[int(pb->Var6)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var6)] +=    1; /* lokal : wie oft    "      */
          }
          if(pb->Var7)
          {
           pVar[int(pb->Var7)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var7)] +=    1; /* lokal : wie oft    "      */
          }
          if(pb->Var8)
          {
           pVar[int(pb->Var8)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var8)] +=    1; /* lokal : wie oft    "      */
          }
          if(pb->Var9)
          {
           pVar[int(pb->Var9)] |= 0x80; /* global: wurde beschrieben */
           pVar[int(pb->Var9)] +=    1; /* lokal : wie oft    "      */
          }
  break;
  }
 }
 // Offene Eingänge suchen für Vxx; Rxx und Xxx können extern beschrieben werden.
 // Der Test kann nicht erkennen, wenn eine Variable zu spät beschrieben wird.
 // Ursache: Es sind alle wilden Sprünge möglich.
 for(pb = pbPC, bn = 1; nextBlk(pb)->Modul != bError;  bn+= pb->Link, pb+= pb->Link) {
   relBlocknummer   = relBlocknr[pb - pbPC];
   Pruefschrittnummer = Pruefschrittnr[pb - pbPC];
   if( pb->Modul == bVorgabe) sn = pb->Intern;
   if( pb->Modul == bScript) {
     for(ptPR = pParRef; ptPR < pPR; ptPR++) {
      // Nur Ziele
      if((ptPR->t == 'v') && (ptPR->sn == (short)sn) && (ptPR->rbn == relBlocknummer) ) {
        i = ptPR->pn;
        if( resvar ) {
          // Test gegen Ressourcenvariable, die gelesen werden können
          tmp.Format(_T("v%02d "), i);
          if( _tcsstr(resvar, tmp) != NULL )
            continue;
        }
        if( !pVar[i] && i <= maxVar ) { // externe Variable X** werden nicht geprüft
          ldstr.LoadString(IDS_STRING79);
          _stprintf_s(buf, maxbuf, ldstr, i);
          semerr(bn, sn, buf);
        }
      }
    }
  
   } else {
    for(j = 0, pf = (float*)&(pb->Var1); j < maxBefVar; j++, pf++) {  
      if(isNAN(pf)) {
        // Variable
        i = getNAN(pf);
        if( resvar ) {
          // Test gegen Ressourcenvariable, die gelesen werden können
          tmp.Format(_T("v%02d "), i);
          if( _tcsstr(resvar, tmp) != NULL )
            continue;
        }
        if( !pVar[i] && i <= maxVar ) { // externe Variable X** werden nicht geprüft
          ldstr.LoadString(IDS_STRING37);
          _stprintf_s(buf, maxbuf, ldstr, j+1, i);
          semerr(bn, sn, buf);
        }
      }
    }
   }
 }
 // SHA1 berechnen; Endeblock noch alles null
 {
 CSha1  sha1;
 float  fsha1[8];

 sha1.Init();
 sha1.Update((LPBYTE)pbPC, sizeof(TBefehl) * CounterBefehl);
 sha1.Final();
 sha1.Hash2Float(fsha1);
 sha1str.Format(_T("\nSHA1:\nlocal =%s\n"),sha1.HastStr().GetString());

 for(pb = pbPC, ok = 0; nextBlk(pb)->Modul != bError; pb += pb->Link)
  if(pb->Modul == bEnde) {
    // Konverter-Versionsword in Val2 eintragen
    pb->Val2 = (layout_ver << 8) | konvert_ver_lo;
    // Magic eintragen
    pb->Val3 = Magic;

    pb->Var1 = fsha1[0];
    pb->Var2 = fsha1[1]; 
    pb->Var3 = fsha1[2];
    pb->Var4 = fsha1[3];
    pb->Var5 = fsha1[4];
    pb->Var6 = fsha1[5];
    pb->Var7 = fsha1[6];
    pb->Var8 = fsha1[7];
  }

 // SHA1 über alles
 sha1.Init();
 sha1.Update((LPBYTE)pbPC, sizeof(TBefehl) * CounterBefehl);
 sha1.Final();
 sha1str += CString("global=") + sha1.HastStr() + CString("\n");
 }

 // Programmlänge in Befehlen zurückgeben
 if(fehler) return 0;
 return CounterBefehl;
}

int CKonvert::getSchritt(unsigned int bn)
{
  return pbPC[bn].Intern;
}

int CKonvert::referenz(LPCTSTR resvar)
{
 const int    maxbuf = 256;
 ParRef        *pPRmax;
 TBefehl       *pb;
 int            i, f, sn, sn1, sn2, bn, eincnt, counter, old_sn, old_relBlocknummer, pos, index;
 _TCHAR         buf[maxbuf], ch, einzug[maxbuf], fuell[maxbuf];
 const int    maxCounter = 8;
 CString        label, str, tmp;

 /* ------------------------------ Parameter -------------------------------------------------- */
 ldstr.LoadString(IDS_STRING38);
 fref->WriteString(ldstr);
 pPRmax = pPR;
 for(i = 1; i <= maxPar; i++) {
   // Durchsuche Liste nach diesem Parameter
   f = 1; counter = 0;
   // Überschrift bei erstem Auftreten
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 'p') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("P%03d: "), i);
         fref->WriteString(buf);
         f = 0;
       }
     if( ++counter > maxCounter ) {
      fref->WriteString(_T("\n     ") );
      counter = 1;
     }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
       fref->WriteString(buf);
     }
   }
   // Zeile mit nl abschlieáen
   if(!f) fref->WriteString(_T("\n") );
 }
 fref->WriteString(_T("\n") );

 /* ------------------------------ Globale Parameter ----------------------------------------- */
 // keine gesonderte Überschrift für globale Parameter
 pPRmax = pPR;
 for(i = 1; i <= maxPar; i++) {
   // Durchsuche Liste nach diesem Parameter
   f = 1; counter = 0;
   // Überschrift bei erstem Auftreten
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 'g') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("G%03d: "), i);
         fref->WriteString(buf);
         f = 0;
       }
     if( ++counter > maxCounter ) {
      fref->WriteString(_T("\n     ") );
      counter = 1;
     }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
       fref->WriteString(buf);
     }
   }
   // Zeile mit nl abschlieáen
   if(!f) fref->WriteString(_T("\n") );
 }
 fref->WriteString(_T("\n") );

 /* ------------------------------ Variable -------------------------------------------------- */
 if( resvar == NULL )
  ldstr.LoadString(IDS_STRING39);
 else
  ldstr.LoadString(IDS_STRING77);
 fref->WriteString(ldstr);
 pPRmax = pPR;
 for(i = 1; i <= maxVar; i++) {
   // Durchsuche Liste nach dieser Variablen
   f = 1; counter = 0;
   // Quellen
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 'V') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("V%02d: "), i);
         fref->WriteString(buf);
         f = 0; 
       }
     if( ++counter > maxCounter ) {
      fref->WriteString(_T("\n     ") );
      counter = 1;
     }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
       fref->WriteString(buf);
     }
   }
   if(!f) {
     if( ++counter > maxCounter ) {
         fref->WriteString(_T("\n     ") );
       counter = 1;
     }
     fref->WriteString(_T("   ==>   ") ); 
   }
   // Ziele
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 'v') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten; dürfte hier nicht auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("V%02d:    ==>   "), i);
         fref->WriteString(buf);
         f = 0;
       }
     if( ++counter > maxCounter ) {
         fref->WriteString(_T("\n     ") );
       counter = 1;
     }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
       fref->WriteString(buf);
     }
   }
   // Zeile mit nl abschlieáen
   if(!f) fref->WriteString(_T("\n") );
 }
 fref->WriteString(_T("\n") );

 /* ------------------------------ Retain Variable ------------------------------------------- */
 pPRmax = pPR;
 for(i = 1; i <= maxRVar; i++) {
   // Durchsuche Liste nach dieser Variablen
   f = 1; counter = 0;
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 'R') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("R%02d: ") , i);
         fref->WriteString(buf);
         f = 0; 
       }
     if( ++counter > maxCounter ) {
      fref->WriteString(_T("\n     ") );
      counter = 1;
     }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
       fref->WriteString(buf);
     }
   }
   if(!f) {
     if( ++counter > maxCounter ) {
         fref->WriteString(_T("\n     ") );
       counter = 1;
     }
     fref->WriteString(_T("   ==>   ") ); 
   } 
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 'r') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("R%02d:    <==   "), i);
         fref->WriteString(buf);
         f = 0;
       }
     if( ++counter > maxCounter ) {
         fref->WriteString(_T("\n     ") );
       counter = 1;
     }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
       fref->WriteString(buf);
     }
   }
   // Zeile mit nl abschlieáen
   if(!f) fref->WriteString(_T("\n") );
 }
 fref->WriteString(_T("\n") );

 /* ------------------------------ externe Variable ------------------------------------------- */
 pPRmax = pPR;
 for(i = 1; i <= maxXVar; i++) {
   // Durchsuche Liste nach dieser Variablen
   f = 1; counter = 0;
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 'X') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("X%02d: ") , i);
         fref->WriteString(buf);
         f = 0; 
       }
     if( ++counter > maxCounter ) {
      fref->WriteString(_T("\n     ") );
      counter = 1;
     }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
       fref->WriteString(buf);
     }
   }
   if(!f) {
     if( ++counter > maxCounter ) {
         fref->WriteString(_T("\n     ") );
       counter = 1;
     }
     fref->WriteString(_T("   ==>   ") ); 
   } 
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 'x') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("X%02d:    <==   "), i);
         fref->WriteString(buf);
         f = 0;
       }
     if( ++counter > maxCounter ) {
         fref->WriteString(_T("\n     ") );
       counter = 1;
     }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
       fref->WriteString(buf);
     }
   }
   // Zeile mit nl abschlieáen
   if(!f) fref->WriteString(_T("\n") );
 }
 fref->WriteString(_T("\n") );

 /* ------------------------------ Text-Variable --------------------------------------------- */
 ldstr.LoadString(IDS_STRING75);
 fref->WriteString(ldstr);
 pPRmax = pPR;
 for(i = 1; i <= maxVar; i++) {
   // Durchsuche Liste nach dieser Variablen
   f = 1; counter = 0;
   // Quellen
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 'T') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("T%02d: "), i);
         fref->WriteString(buf);
         f = 0; 
       }
     if( ++counter > maxCounter ) {
      fref->WriteString(_T("\n     ") );
      counter = 1;
     }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
       fref->WriteString(buf);
     }
   }
   if(!f) {
     if( ++counter > maxCounter ) {
         fref->WriteString(_T("\n     ") );
       counter = 1;
     }
     fref->WriteString(_T("   ==>   ") ); 
   }
   // Ziele
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 't') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten; dürfte hier nicht auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("T%02d:    ==>   "), i);
         fref->WriteString(buf);
         f = 0;
       }
     if( ++counter > maxCounter ) {
         fref->WriteString(_T("\n     "));
       counter = 1;
     }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
       fref->WriteString(buf);
     }
   }
   // Zeile mit nl abschlieáen
   if(!f) fref->WriteString(_T("\n") );
 }
 fref->WriteString(_T("\n") );
 /* ------------------------------ Ressource --------------------------------------------- */
 if( resvar ) {
  // Ressourcenvariable:\n%s\n
  tmp.LoadString(IDS_STRING103);
  str.Format(tmp, resvar);
  fref->WriteString(str); 
 }
 fref->WriteString(_T("\n") );

 /* ------------------------------ Label ------------------------------------------------- */
 ldstr.LoadString(IDS_STRING60);
 fref->WriteString(ldstr);
 label = LabelStr;
 while( (pos = label.Find(':')) != -1 ) {
     label = label.Mid(pos+1);
     pos   = label.Find('#');
     str   = label.Left(pos);
     label = label.Mid(pos+1);
     i     = _tstoi(label);
     _stprintf_s(buf, maxbuf, _T("%4.4s   (%3d)\n"), str.GetBuffer(), i);
     fref->WriteString(buf);
 }
 fref->WriteString(_T("\n") );
 LabelStr.MakeLower();
 
 /* ------------------------------ Rufe -------------------------------------------------- */
 if( variante & VARIANTE_OPTION_02) 
   ldstr.LoadString(IDS_STRING110);
 else
   ldstr.LoadString(IDS_STRING40);
 fref->WriteString(ldstr);
 pPRmax = pPR;
 // für alle möglichen Schritte
 for(i = 1; i <= (int)CounterSchritte; i++) {
   // Durchsuche Liste nach diesem Schrittziel
   f = 1; counter = 0;
   // Überschrift bei erstem Auftreten
   for(pPR = pParRef; pPR < pPRmax; pPR++) {
     if((pPR->t == 'c') && (pPR->pn == i)) {
       // Überschrift bei erstem Auftreten
       if(f) {
         _stprintf_s(buf, maxbuf, _T("UP(%4d) <= "), i);
         fref->WriteString(buf);
         f = 0;
       }
     if( ++counter > maxCounter ) {
         fref->WriteString(_T("\n            ") );
       counter = 1;
     }
     if( variante & VARIANTE_OPTION_02) { 
      switch(pPR->bed) {
      case 1:   tmp = CString("IO ");  break;
      case 2:   tmp = CString("NIO");  break;
      case 3:   tmp = CString("UNB");  break;
      default:  tmp = CString("???");  break;
      }
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d,%s) "), pPR->sn, pPR->rbn, tmp.GetBuffer());
     } else {
       _stprintf_s(buf, maxbuf, _T("(%3d,%2d) "), pPR->sn, pPR->rbn);
     }
       fref->WriteString(buf);
     }
   }
   // Zeile mit nl abschließen
   if(!f) fref->WriteString(_T("\n") );
 }
 fref->WriteString(_T("\n") );

 /* ------------------------------ Sprungwarnungen --------------------------------------------------- */
 ldstr.LoadString(IDS_STRING111);
 fref->WriteString(ldstr);
 // Rücksprungliste initialisieren
 cntRJmp = 0; // Counter Liste leer
 for(pb = pbPC; nextBlk(pb)->Modul != bError; pb+= pb->Link) {
  switch(pb->Modul) {
  case bVorgabe:  sn = pb->Intern;
          break;
  case bVerzweigung_GUT_FEHL:
          // Rücksprung IO
          if(pb-pbPC > (int)pb->Val1) 
            addRJmp( sn, getSchritt(pb->Val1) );
          // Rücksprung NIO
          if(pb-pbPC > (int)pb->Val2) 
            addRJmp( sn, getSchritt(pb->Val2) );
          break;
   }
 }

 // Sprünge testen
 for(pb = pbPC; nextBlk(pb)->Modul != bError; pb+= pb->Link) {
  relBlocknummer    = relBlocknr[pb - pbPC];
  buf[0] = '\0';  
  switch(pb->Modul) {
  case bVorgabe:  sn = pb->Intern;
          break;
  case bVerzweigung_GUT_FEHL:
          // Sprung aus Rücksprungschleife ?
          index = 0;
          while( (index=indexRJmp(sn, index, false)) != -1 ) {
            // Aussprung IO
            if( !inRJmp( index, getSchritt(pb->Val1)) ) {
              ldstr.LoadString(IDS_STRING112);
              _stprintf_s(buf, maxbuf, ldstr, pRJmp[index].rsn, pRJmp[index].sn, sn, relBlocknummer, getSchritt(pb->Val1)); 
              fref->WriteString(buf);
              fref->WriteString(_T("\n") );
            }
            // Aussprung NIO
            if( !inRJmp( index, getSchritt(pb->Val2)) ) {
              ldstr.LoadString(IDS_STRING112);
              _stprintf_s(buf, maxbuf, ldstr, pRJmp[index].rsn, pRJmp[index].sn, sn, relBlocknummer, getSchritt(pb->Val2)); 
              fref->WriteString(buf);
              fref->WriteString(_T("\n") );
            }
            index++;
          }
          break;
  case bVerzweigung_Pruefnummer:
          // Sprung aus Rücksprungschleife ?
          index = 0;
          while( (index=indexRJmp(sn, index, false)) != -1 ) {
            if( !inRJmp( index, getSchritt(pb->Val1)) ) {
              ldstr.LoadString(IDS_STRING112);
              _stprintf_s(buf, maxbuf, ldstr, pRJmp[index].rsn, pRJmp[index].sn, sn, relBlocknummer, getSchritt(pb->Val1)); 
            }
            fref->WriteString(buf);
            fref->WriteString(_T("\n") );
            index++;
          }
          break;
  }
 }

 /* ------------------------------ Wiederholwarnungen ------------------------------------------------ */
 // Loopliste initialisieren
 cntRJmp = 0; // Counter Liste leer
 for(pb = pbPC; nextBlk(pb)->Modul != bError; pb+= pb->Link) {
  switch(pb->Modul) {
  case bVorgabe:  sn = pb->Intern;
          break;
  case bWiederholfunktion:
          addRJmp( pb->Val3, sn+1 );
          break;
  }
 }

 // Sprünge testen
 for(pb = pbPC; nextBlk(pb)->Modul != bError; pb+= pb->Link) {
  relBlocknummer    = relBlocknr[pb - pbPC];
  buf[0] = '\0';  
  switch(pb->Modul) {
  case bVorgabe:  sn = pb->Intern;
          break;
  case bVerzweigung_GUT_FEHL:
          // Sprung aus Rücksprungschleife ?
          index = 0;
          while( (index=indexRJmp(sn, index, false)) != -1 ) {
            // Aussprung IO
            if( !inRJmp( index, getSchritt(pb->Val1)) ) {
              ldstr.LoadString(IDS_STRING112);
              _stprintf_s(buf, maxbuf, ldstr, pRJmp[index].rsn, pRJmp[index].sn, sn, relBlocknummer, getSchritt(pb->Val1)); 
              fref->WriteString(buf);
              fref->WriteString(_T("\n") );
            }
            // Aussprung NIO
            if( !inRJmp( index, getSchritt(pb->Val2)) ) {
              ldstr.LoadString(IDS_STRING112);
              _stprintf_s(buf, maxbuf, ldstr, pRJmp[index].rsn, pRJmp[index].sn, sn, relBlocknummer, getSchritt(pb->Val2)); 
              fref->WriteString(buf);
              fref->WriteString(_T("\n") );
            }
            index++;
          }
          break;
  case bVerzweigung_Pruefnummer:
          // Sprung aus Rücksprungschleife ?
          index = 0;
          while( (index=indexRJmp(sn, index, false)) != -1 ) {
            if( !inRJmp( index, getSchritt(pb->Val1)) ) {
              ldstr.LoadString(IDS_STRING112);
              _stprintf_s(buf, maxbuf, ldstr, pRJmp[index].rsn, pRJmp[index].sn, sn, relBlocknummer, getSchritt(pb->Val1)); 
              fref->WriteString(buf);
              fref->WriteString(_T("\n") );
            }
            index++;
          }
          break;
  }
 }
 fref->WriteString(_T("\n") );


/* ------------------------------ Sprungreferenzen 1 ------------------------------------------------ */
 // Rücksprünge aufsammeln
 for(i = 1; i < (int)CounterSchritte; i++) 
  pSteps[i] = 0;
 for(pb = pbPC; nextBlk(pb)->Modul != bError; pb+= pb->Link) {
  switch(pb->Modul) {
  case bVorgabe:  sn = pb->Intern;
          break;
  case bVerzweigung_GUT_FEHL:
          // Rücksprünge in Stepliste eintragen
          if(pb-pbPC > (int)pb->Val1) 
            pSteps[sn] = getSchritt(pb->Val1);
          if(pb-pbPC > (int)pb->Val2) 
            pSteps[sn] = getSchritt(pb->Val2);
          break;
  }
 }

 
 einzug[0] = '\0';
 for(i = 0; i < maxbuf; i++) fuell[i] = ' ';
 fuell[10] = '\0';
 eincnt    = 0;
 ldstr.LoadString(IDS_STRING41);
 fref->WriteString(ldstr);
 for(pb = pbPC; nextBlk(pb)->Modul != bError; pb+= pb->Link) {
  relBlocknummer    = relBlocknr[pb - pbPC];
  Pruefschrittnummer  = Pruefschrittnr[pb - pbPC];
  buf[0] = '\0';
  switch(pb->Modul) {
  case bVorgabe:  sn = pb->Intern;
          // Rücksprünge zu diesem Schritt ?
          callback(sn);
          if(pb->Err) {
            if(eincnt != 0) {
              eincnt -= 2;
              einzug[eincnt] = '\0';
              for(i = 0; i < maxbuf; i++) fuell[i] = ' ';
              fuell[10-eincnt] = '\0';
            } else {
              // Stop-Error
              tmp.LoadString(IDS_STRING105);
              _tcscat_s(buf, maxbuf, tmp);  
            }
            ldstr.LoadString(IDS_STRING42);
            _stprintf_s(buf, maxbuf, ldstr, einzug, fuell, sn, getLabel(sn).GetString());
          }
          break;
  case bVerzweigung_GUT_FEHL:
          // absolute Blocknummer für akt.Modul und Ziele testen
          if((pb-pbPC > (int)pb->Val1) || (pb-pbPC > (int)pb->Val2)) ch = 'R';
          else ch = ' ';
          if(pb[-1].Modul == bVorgabe) {  // Vorgängerbestimmung stets richtig, da Block-Link=1
            ldstr.LoadString(IDS_STRING43);
            _stprintf_s(buf, maxbuf, ldstr, einzug, fuell, sn, relBlocknummer, getSchritt(pb->Val1), getLabel(pb->Val1).GetString(), getSchritt(pb->Val2), getLabel(pb->Val2).GetString(), ch);
          }
          else {
            ldstr.LoadString(IDS_STRING44);
            _stprintf_s(buf, maxbuf, ldstr, einzug, fuell, sn, relBlocknummer, getSchritt(pb->Val1), getLabel(pb->Val1).GetString(), getSchritt(pb->Val2), getLabel(pb->Val2).GetString(), ch);
          }
          break;
  case bVerzweigung_Pruefnummer:
          ldstr.LoadString(IDS_STRING45);
          _stprintf_s(buf, maxbuf, ldstr, einzug, fuell, sn, relBlocknummer, getSchritt(pb->Val1), getLabel(pb->Val1).GetString());
          break;
  case bWiederholfunktion:
          ldstr.LoadString(IDS_STRING46);
          _stprintf_s(buf, maxbuf, ldstr, einzug, fuell, sn+1, getLabel(sn+1).GetString());
          eincnt += 2;
          if(eincnt > 10) {
            ldstr.LoadString(IDS_STRING47);
            _tcscat_s(buf, maxbuf, ldstr);
          }
          else {
            for(i = 0; i < maxbuf; i++) einzug[i] = ' ';
            einzug[eincnt]   = '\0';
            fuell[10-eincnt] = '\0';
          }
          break;
  }
  if(buf[0]) {
    fref->WriteString(buf);
    fref->WriteString(_T("\n") );
  }
 }
 fref->WriteString(_T("\n") );

 /* ------------------------------ Sprungreferenzen 2 ------------------------------------------------ */
 tmp.LoadString(IDS_STRING105);
 fref->WriteString(tmp);
 for(pb = pbPC; nextBlk(pb)->Modul != bError; pb+= pb->Link) {
  relBlocknummer    = relBlocknr[pb - pbPC];
  Pruefschrittnummer  = Pruefschrittnr[pb - pbPC];
  buf[0] = '\0';
  switch(pb->Modul) {
  case bVorgabe:  sn  = pb->Intern;
          bn  = pb - pbPC;
          break;
  case bVerzweigung_GUT_FEHL:
          if(pb[-1].Modul == bVorgabe) {  // Vorgängerbestimmung stets richtig, da Block-Link=1
            ldstr.LoadString(IDS_STRING106);
            sn1 = getSchritt(pb->Val1);
            sn2 = getSchritt(pb->Val2);
            _stprintf_s(buf, maxbuf, ldstr, Comments[bn].GetString(), sn, relBlocknummer, sn1, Comments[pb->Val1].GetString(), sn2, Comments[pb->Val2].GetString());
          }
          else {
            ldstr.LoadString(IDS_STRING107);
            sn1 = getSchritt(pb->Val1);
            sn2 = getSchritt(pb->Val2);
            _stprintf_s(buf, maxbuf, ldstr, Comments[bn].GetString(), Comments[(pb-1) - pbPC].GetString(), sn, relBlocknummer, sn1, Comments[pb->Val1].GetString(), sn2, Comments[pb->Val2].GetString());
          }
          break;
  case bVerzweigung_Pruefnummer:
            ldstr.LoadString(IDS_STRING108);
            sn1 = getSchritt(pb->Val1);
            _stprintf_s(buf, maxbuf, ldstr, Comments[bn].GetString(), Comments[(pb-1) - pbPC].GetString(), sn, relBlocknummer, sn1, Comments[pb->Val1].GetString());
          break;
  case bWiederholfunktion:
            ldstr.LoadString(IDS_STRING109);
            sn1 = getSchritt(pb->Val1);
            sn2 = getSchritt(pb->Val3);
            _stprintf_s(buf, maxbuf, ldstr, Comments[bn].GetString(), pb->Val2, sn, relBlocknummer, sn1, Comments[pb->Val1].GetString(), sn2, Comments[pb->Val3].GetString());
          break;
  }
  if(buf[0]) {
    fref->WriteString(buf);
    fref->WriteString(_T("\n") );
  }
 }
 fref->WriteString(_T("\n") );


 /* ------------------------------ Scriptreferenzen -------------------------------------------------- */
 
 // Scripte suchen
 ldstr.LoadString(IDS_STRING58);
 fref->WriteString(ldstr);
 for(pb = pbPC; nextBlk(pb)->Modul != bError; pb += pb->Link) {
  relBlocknummer    = relBlocknr[pb - pbPC];
  Pruefschrittnummer  = Pruefschrittnr[pb - pbPC];
    if( pb->Modul == bVorgabe ) {
        sn = pb->Intern;
    }

    if( pb->Modul == bScript ) {
        // Script gefunden , Erweiterungen testen
    ldstr.LoadString(IDS_STRING61);
        f = pb->Link-1; 
        _stprintf_s(buf, maxbuf, ldstr, sn, relBlocknummer, f);
        fref->WriteString(buf);
    
    // Adress-Referenz
    tmp = CString("  ");
    for( int index = 0; index < BlockLineAddr.GetCount(); index +=3 ) {
      if(pb - pbPC == BlockLineAddr.GetAt(index) ) {
        int line =  BlockLineAddr.GetAt(index + 1);
        int addr =  BlockLineAddr.GetAt(index + 2);
        _stprintf_s(buf, maxbuf, _T("(%d:%d)"), line, addr);
        tmp += buf;
      }
    }
        fref->WriteString(tmp + CString("\n") );

        old_sn = sn; old_relBlocknummer = relBlocknummer;
    }
 }

 fref->WriteString(sha1str);
 fref->WriteString(_T("\n") );

 // PTB
 if( m_rPTB != 1.0f ) {
   tmp.Format(_T("PTB   = %.2fµs\n"), 1000.0f/m_rPTB);
     fref->WriteString(tmp);
 }

 return 1;
}

void CKonvert::callback(int snr)
{
  const int maxbuf = 256;
  int   i;
  bool  f;
  _TCHAR  buf[maxbuf];

  f = false;
  for(i = 1; i < (int)CounterSchritte; i++) 
    if(pSteps[i] == snr) {
      if(f) fref->WriteString(_T(",") );
      f = 1;
      _stprintf_s(buf, maxbuf, _T("R%d"), i);
      fref->WriteString(buf);
    }
  if(f) 
    fref->WriteString(_T(":\n") );
}

// Rücksprungloop hinzufügen
 void CKonvert::addRJmp(int _sn, int _rsn)
{ 
  if( cntRJmp >= maxBefehle/2 ) return; // Sollte nie erreicht werden.
  pRJmp[cntRJmp].sn  = (short)_sn; 
  pRJmp[cntRJmp].rsn = (short)_rsn; 
  cntRJmp++;
}

// return Index des umschließenden Rücksprungloop 
// Suche ab _index; init=0
// -1 = kein Index gefunden
// withR schließt Rücksprungschritt selbst mit ein
int CKonvert::indexRJmp(int _sn, int _index, bool withR)
{
  int   i;

  if( withR ) {
    for( i = _index; i < cntRJmp; i++ )
      if(  pRJmp[i].rsn <=  (short)_sn && (short)_sn  <= pRJmp[i].sn ) return i;
  } else {
    for( i = _index; i < cntRJmp; i++ )
      if(  pRJmp[i].rsn <=  (short)_sn && (short)_sn  <  pRJmp[i].sn ) return i;
  }
  return -1;
}

// Schritt im Rücksprungloop enthalten ?
bool CKonvert::inRJmp( int index, int _sn )
{
  if( index < 0 || index >= cntRJmp ) return false;
  return  pRJmp[index].rsn <=  (short)_sn && (short)_sn  <= pRJmp[index].sn ;
}

CString CKonvert::getExtValue(int bnr, int vnr)
{
  TBefehl*  pb;
  CString   str;
  double    x;
  
  str = CString("??@??");
  if( bnr < 0 || bnr >= CounterBefehl ) return str;

  pb = &pbPC[bnr];

  vnr -= 8;   // Spaltennummer val1 - Index val1 im Befehl
  switch( vnr )
  {
  case  0: x = pb->Val1; break;
  case  1: x = pb->Val2; break;
  case  2: x = pb->Val3; break;
  case  3: x = pb->Val4; break;
  case  4: x = pb->Var1; break;
  case  5: x = pb->Var2; break;
  case  6: x = pb->Var3; break;
  case  7: x = pb->Var4; break;
  case  8: x = pb->Var5; break;
  case  9: x = pb->Var6; break;
  case 10: x = pb->Var7; break;
  case 11: x = pb->Var8; break;
  case 12: x = pb->Var9; break;
  default: return str;
  }

  str.Format(_T("%g"), x);
  return str;
}

CString  CKonvert::getExtText(int bnr, int vnr, bool global)
{
  CString   str;
  
  str = CString("??@??");
  if( bnr < 0 || bnr >= CounterBefehl ) return str;

  if( global ) {
    if( vnr < 0 || vnr >= maxPar ) return str;
  } else {
    if( vnr < 0 || vnr >= maxPar ) return str;
    str = pTPar[vnr].text;
  }
  str = CString('\"') + pGTPar[vnr].text + CString('\"');
  str.Replace(_T("\"\""), _T("\"") );

  return str;
}
