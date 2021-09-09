#ifndef _Konverterversion_
#define _Konverterversion_
/* Konverterversion */
// Layout-Versionen 
// werden unterstützt 
// Die Unterscheidung erfolgt über den Varianten-Schalter
// Subversion: ".6" : Behandlung der bStart-Sonderfälle
// >= 128 Systeme ab Beckhoff MULTI
#define KONVERTER_VERSIONS_STR	_T(" 1, 2, 3, 4, 5, 6, 7, 8, 9,10,")\
                                _T("11,12,13,14,15,16,17,18,19,20,")\
                                _T("21,22,23,24,25,26,27,28,29,30,")\
								_T("31,32,33,34,35,36,37,38,39,40,")\
								_T("41,42,43,44,45,46,47,48,49,50,")\
								_T("51,52,53,54,55,56,57,58,59,60,")\
								_T("61,62,63,64,65,66,67,68,69,70,")\
								_T("71,72,73,74,75,76,77,78,79,80,")\
								_T("81,82,83,84,85,86,87,88,89,90,")\
								_T("91,92,93,94,95,96,97,98,99,")\
								_T("128,129,")\
								_T("130,131,132,133,134,135,136,137,138,139,")\
								_T("140,141,142,143,144,145,146,147,148,149,")\
								_T(".6,")


#include <layout/variante.h>
#include "stdafx.h"
#include <layout/layout.h>


typedef unsigned char      Usint; 
typedef unsigned short int Uint;
typedef unsigned long  int Udint;
typedef unsigned char      Bool;
typedef float              Real;


const int 
        maxBefehle =   4096,							/* µµ */
		maxPar     =     CLayout::MAXINDEXP,
		maxTextLen =     64, 
		maxVARS    =    256,
        maxVar     =    CLayout::MAXINDEXV,
        maxRVar    =    CLayout::MAXINDEXV,
        maxXVar    =    CLayout::MAXINDEXX,
		offXVar    =	CLayout::MAXINDEXV,				/* >= maxVar */
		offRVar    =    128,
        maxrBlk    =     25,
        maxBuf     =    255,
        maxLine    =    512,
        maxHistorie=      2,
        maxParRef  =   8000,
		maxBefVar  =      9;				/* Anzahl der Var im Befehl */

enum  TBlock    {
/*BEGIN_WIZARD_ENUM_BLOCK*/
        bError,                             /*   0  */
        bStart,                             /*   1  */
        bToleranz,                          /*   2  */
        bToleranz_Differenz,                /*   3  */
        bLeistungspruefung,                 /*   4  */
        bLeistungspruefung_Differenz,       /*   5  */
        bToleranzpruefung_Vergleich,        /*   6  */
        bDigitalwert,                       /*   7  */
        bDigitalwert_Flanke,                /*   8  */
        bStabilisierung,                    /*   9  */
        bArithmetikblock_Verknuepfungen,    /*  10  */
        bVerzweigung_GUT_FEHL,              /*  11  */
        bVerzweigung_Pruefnummer,           /*  12  */
        bWiederholfunktion,                 /*  13  */
        bUnterprogrammruf,                  /*  14  */
        bUnterprogrammruecksprung,          /*  15  */
        bKorrekturblock_Messsignal,         /*  16  */
        bKorrekturblock_Vorgabesignal,      /*  17  */
        bProgrammabbruch,                   /*  18  */
        bKonstante,                         /*  19  */
        bEnde,                              /*  20  */
        bToleranzpruefung_Knickpunkt,       /*  21  */
        bMaximum,                           /*  22  */
        bMinimum,                           /*  23  */
        bHistorie,                          /*  24  */
        bArithmetikblock_Statistik,         /*  25  */
        bAnzeige,                           /*  26  */
        bFilter_Messsignal,                 /*  27  */
        bParametrierblock_Messkreis,        /*  28  */
        bParametrierblock_Generator,        /*  29  */
        bParametrierblock_Regelkreis,       /*  30  */
        bParametrierblock_Ausgabe,          /*  31  */
        bParametrierblock_Zeitgeber,        /*  32  */
		bKonstantenfeld,				    /*  33  */
		bKonstantenfeld_Erweiterung,	    /*  34  */
        bSchnelle_Leistungspruefung,        /*  35  */
        bArithmetikblock_Logik,             /*  36  */
        bScript,                            /*  37  */
        bScript_Erweiterung,                /*  38  */
        bSchnelle_Toleranz,                 /*  39  */
        bFrontEnd1,                         /*  40  */
        bFrontEnd2,                         /*  41  */
        bSchwellzeit,                       /*  42  */
        bToleranzpruefung_Schwellzeit,      /*  43  */
		bArchivierung,					    /*  44  */
		bArchivauswertung,                  /*  45  */
	    bAbfrage,							/*  46  */
		bTextverarbeitung,					/*  47  */
		bParametrierbock,					/*  48  */ 
        bFrontEnd3,                         /*  49  */
        bFrontEnd4,                         /*  50  */
        bFrontEnd5,                         /*  51  */
        bFrontEnd6,                         /*  52  */
        bFrontEnd7,                         /*  53  */
        bFrontEnd8,                         /*  54  */
		bRetain,							/*  55  */
		bAbfrageValue,						/*  56  */
		bAbfrageDlg,						/*  57  */
		bAbfrageSignal,						/*  58  */
/*END_WIZARD_ENUM_BLOCK*/
        bMax                               
};
#define bVorgabe bStart					// Beide Konstanten stehen für Vorgabeblock

typedef Uint Tblock;					// Damit Modul 16 Bit breit ist.
#pragma pack(4)							// Ausrichtung mit SPC dword-allignement gleichsetzen
struct TBefehl {						/* µµ	*/
/*BEGIN_WIZARD_BEFEHL*/
	Tblock Modul;
	Uint   Store;
	Bool   Err;
	Usint  Link;
	Uint   Intern;
	Udint  Val1;
	Udint  Val2;
	Udint  Val3;
	Udint  Val4;
	Real   Var1;
	Real   Var2;
	Real   Var3;
	Real   Var4;
	Real   Var5;
	Real   Var6;
	Real   Var7;
	Real   Var8;
	Real   Var9;
	Bool   B1;
	Bool   B2;
	Bool   B3;
	Bool   B4;
/*END_WIZARD_BEFEHL*/
};
#pragma pack()

#define BefehlBaseSize 8				// ( &(((TBefehl*)0)->Val1) - &(((TBefehl*)0)->Modul) )

struct ParRef {
 _TCHAR	t;								// Typ Par.:'p', Quelle:'V', Senke:'v', Call:'c'
										//               Quelle:'X', Senke:'x', 
										//               Quelle:'T', Senke:'t', 
 int	  rbn;							// relative Blocknummer
 int	  bed;							// Call von io oder nio io=1 nio=2
 short    pn;                           // Parameter- Variablen- Callnummer
 short    sn;                           // Schrittnummer

};

struct TextPar	{
	_TCHAR		text[maxTextLen+1];
};

struct	RJmp {
	short	sn;							// Schrittnummer eines Rücksprungs
	short	rsn;						// Schrittnummer dessen Rücksprungziels
};

// Im Prüfplan kann immer nur ein Block erweitert werden.
class CExtender {
	TBefehl*	pBaseBefehl;			// Zeiger auf aktuellen zu erweiternden Block 
	CByteArray	Buffer;
public:
	CExtender()	{reset();}
	
	void		reset();
	CString		check(void);
	CString		convert(TBefehl* pBefehl);
	void		init( TBefehl*	pBefehl, CByteArray& rBuffer);
};

class CUtf8File;
class CKonvert {
 enum			{maxbuffer = 256};

 int			CounterBefehl;					// Zähler der gelesenen Befehle
 int			CounterPar;						// Zähler der gelesenen Parameter
 int			CounterGPar;					// Zähler der gelesenen globalen Parameter
 unsigned int	CounterSchritte;				// Zähler der gelesenen Schritte
 int			Schrittnummer;					// akt. Schritt
 int			Pruefschrittnummer;				// akt. Prüfschritt
 int			relBlocknummer;					// rel. Blocknummer (Vorgabe = 1)
 int			Zeile;							// aktuelle Zeile
 _TCHAR			Trenner;						// Trennzeichen der Spalten
 int			fehler;							// Fehlerzähler
 CUtf8File		*ferr;							// Filepointer
 CUtf8File		*fref;							// Filepointer
 int			spalte;							// akt. Spalte
 TBefehl*		pBefehl;						// akt. Befehl
 TBefehl*		pbPC;							// Zeiger auf Befehlsspeicher
 float*			pPar;							// Zeiger auf Parameterspeicher
 float*			pGPar;							// Zeiger auf globalen Parameterspeicher
 TextPar*		pTPar;							// Zeiger auf Textparameter
 TextPar*		pGTPar;							// Zeiger auf globale Textparameter
 UINT8*			pVar;							// Zeiger auf VariablenFlagfeld
 short*			pSchritte;						// Blocknummern der Schritte
 short*			pSteps;							// Schrittnummer eines Rücksprungs
 RJmp*			pRJmp;							// Feld für Rücksprunganalyse
 int			cntRJmp;						// Counter für RJmp Einträge
 LPTSTR 		pLine;							// Zeiger in akt. Zeile
 LPCTSTR 		pSignale;						// Beschreibung der Signale
 _TCHAR			buffer[maxbuffer];				// Puffer
 _TCHAR			vtype;							// Varx Typ: c, p, v
 ParRef			*pParRef;						// Parameter-Referenz-Puffer
 ParRef			*pPR;							// Zeiger in ParRefPuffer
 CString		ldstr;							// allg. String für LoadString
 CString		strHelp7;						// Help7 sichern
 Uint			Magic;							// Kennzahl für akt.Quelle	
 int			variante;						// Variante aus theApp
 float			m_rPTB;							// 1000.0 / PTB
 LPCTSTR 		lstname;						// Dateiname ohne Ext.; ScriptListDatei
 UINT8			Schritttyp;						// 1 = Prüfblockgesteuert
 int			relBlocknr[maxBefehle];			// relative Blocknummern
 int			Pruefschrittnr[maxBefehle];		// Prüfschrittnummern
 CWordArray		BlockLineAddr;					// Speicher für Blocknummer, Zeile, Codeadresse
 CString		LabelStr;						// Label Schrittnummer Zuordnung
 int			TotalSchritte;					// Gesamtzahl der Schritte
 unsigned long	Ventil6;						// Ventilvariable Var6
 CExtender		Extender;						// Verwaltung von Erweiterungen
 CStringArray	Comments;						// VB bzw. IO-Kommentare
 CString		sha1str;						// SHA1 String in fexadezimaler Darstellung

 void     error(LPCTSTR);
 void     pref(LPCTSTR);
 void     semerr(int , int , LPCTSTR);
 int	  checkPtr(LPTSTR p);
 Bool     getBool(void);
 Uint     getUint(void);
 Udint    getUdint(void);
 Uint     getHelp(int i);
 Real     getVar(void);
 CString  getTPar(CString&, int);
 Uint     getVal1V(void);
 Uint     getStore(void);
 int      skip(LPTSTR dest = NULL, int maxlen = 0);
 int	  skip0(LPTSTR* dest);
 LPTSTR   start(void);
 void     k2p(LPTSTR);
 float    setNAN(unsigned short);
 int      getNAN(float* f); 
 int      isNAN(float*);
 int      isVal(float);
 void	  storeRef(_TCHAR typ, int pnr, int rbnr, int snr, int ps, int cc = 0);
 int	  getSchritt(unsigned int);
 int      ReplaceLabel(CString& prg);
 int      KonvertLabel(LPCTSTR p);
 CString  getLabel(int sn);
 void	  callback(int snr);
 void	  clear(void);
 void     konvertTime(float& x, bool inverse = false);
 unsigned long	  getExp1(LPTSTR &p);
 unsigned long	  getExp2(LPTSTR &p);
 unsigned long	  getExp3(LPTSTR &p);
 int      readTmpLstFile(LPCTSTR lstname, int bnr);
 int      konPar(LPCTSTR , int counter, bool global);
 __forceinline
 TBefehl* nextBlk(TBefehl* pb) { return &pb[pb->Link];}
public:
 CKonvert();
 ~CKonvert();
 int	  init(_TCHAR t, CUtf8File* _fref, CUtf8File* _ferr, int v, LPCTSTR _pSignale, LPCTSTR _lstname);
 TBefehl* bPC(void) {return pbPC;}
 int      readBefehl(LPCTSTR);
 int      append(void);
 int      readPar(LPCTSTR);
 int      readGPar(LPCTSTR);
 int      check(LPCTSTR, bool DeepTest=true);
 int      referenz(LPCTSTR resvar);
 void	  setMagic(Uint magic) {Magic = magic;}
 void     setLabel(LPCTSTR); 
 void     setPTB( float rPTB) {m_rPTB = rPTB;}
 int      getAnzahl(void) {return fehler ? 0 : CounterBefehl;}
 CWordArray&    getBlockLineAddr() {return BlockLineAddr;}
 void	  addRJmp(int _sn, int _rsn);
 int 	  indexRJmp(int _sn, int _index, bool withR);
 bool	  inRJmp( int index, int _sn );
 CString  getExtValue(int bnr, int vnr);
 CString  getExtText(int bnr, int vnr, bool global);
 void     exterr(int , int , LPCTSTR);
};

#endif
