#include <chaiscript/chaiscript.hpp>
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <streambuf>

#include <algorithm> 
#include <cctype>
#include <locale>

// trim from start (in place)
static inline void ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    }));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
    ltrim(s);
    rtrim(s);
}

// trim from start (copying)
static inline std::string ltrim_copy(std::string s) {
    ltrim(s);
    return s;
}

// trim from end (copying)
static inline std::string rtrim_copy(std::string s) {
    rtrim(s);
    return s;
}

// trim from both ends (copying)
static inline std::string trim_copy(std::string s) {
    trim(s);
    return s;
}

std::vector<std::string> split(const std::string& value, char separator)
{
    std::vector<std::string> result;
    std::string::size_type p = 0;
    std::string::size_type q;
    while ((q = value.find(separator, p)) != std::string::npos) {
        result.emplace_back(value, p, q - p);
        p = q + 1;
    }
    result.emplace_back(value, p);
    return result;
}

struct Command {
    uint16_t module;
    uint16_t store;
    bool error;
    uint8_t link;
    uint16_t intern;
    uint32_t val1;
    uint32_t val2;
    uint32_t val3;
    uint32_t val4;
    float var1;
    float var2;
    float var3;
    float var4;
    float var5;
    float var6;
    float var7;
    float var8;
    float var9;
    bool b1;
    bool b2;
    bool b3;
};

class Fields {
    std::vector<std::string> fields_;
public:
    Fields(std::vector<std::string> fields = std::vector<std::string>())
    : fields_{fields} {}
    std::string getFieldValue(unsigned int index) {
        try {
            return fields_.at(index);
        } catch (std::exception& e) {
            std::cerr << e.what() << std::endl;
        }
        return "";
    }
    // Blocknummer
    unsigned int getBlockNumber() {
        return std::stoi(getFieldValue(0));
    }
    // Block im Schritt
    unsigned int getRelativeBlockNumber() {
        return std::stoi(getFieldValue(1));
    }
    // Schrittnummer
    unsigned int getStepNumber() {
        return std::stoi(getFieldValue(2));
    }
    // Prüfschrittnummer
    unsigned int getTestStepNumber() {
        return std::stoi(getFieldValue(3));
    }
    // Prüfschritt
    unsigned int getBlockType() {
        return std::stoi(getFieldValue(4));
    }
    // Blockname
    std::string getBlockName() {
        return getFieldValue(5);
    }
    // Kommentar
    std::string getComment() {
        return getFieldValue(6);
    }
    // Kommentar Fehler
    std::string getErrorComment() {
        return getFieldValue(7);
    }
    // Val1
    unsigned long getVal1() {
        std::string s{getFieldValue(8)};
        chaiscript::ChaiScript chai;
        chai.add(chaiscript::var<int>(33), "P01");
        auto r = chai.eval(s);
        
        return chai.boxed_cast<int>(r);
    }
    // Val2
    unsigned long getVal2() {
        std::string s{getFieldValue(9)};
        return std::stoi(s);
    }
    // Val3
    unsigned long getVal3() {
        std::string s{getFieldValue(10)};
        return std::stoi(s);
    }
    // Val4
    unsigned long getVal4() {
        std::string s{getFieldValue(11)};
        return std::stoi(s);
    }
};

// map<Blockname, Script>
std::map<std::string, std::string> readIni(const std::string& fileName)
{
    std::ifstream ifs(fileName);
    // std::string chunk((std::istreambuf_iterator<char>(t)),
    //     std::istreambuf_iterator<char>());
    std::map<std::string, std::string> map;
    std::string line;
    while (std::getline(ifs, line)) {
        trim(line);
        if (line.find("[") == 0) { // [] gefunden
            std::vector<std::string> fields = split(line, ';');
            std::string blockName{fields[0].erase(0, 1)};
            std::string script;
            while (std::getline(ifs, line)) {
                trim(line);
                if (line.find("//{") != std::string::npos) {
                    while (std::getline(ifs, line)) {
                        if (line.find("//}") != std::string::npos) {
                            map[blockName] = script;
                            break;
                        }
                        script += line + '\n';
                    }
                } else if (line == ".") {
                    break;
                }
            }
        }
    }
    return map;
}

std::vector<Fields> readPP0(const std::string& fileName)
{
    std::ifstream ifs(fileName);
    std::string chunk((std::istreambuf_iterator<char>(ifs)),
        std::istreambuf_iterator<char>());

    std::vector<Fields> lines;
    std::vector<std::string> l{split(chunk, '\n')};

    for (auto ll : l) {
        lines.push_back(split(ll, ';'));
    }
    return lines;
}

int main(int argc, char *argv[])
{
    if (argc < 3) {
        return -1;
    }

    std::map<std::string, std::string> chaiFunctions = readIni(argv[1]);
    std::vector<Fields> lines = readPP0(argv[2]);


    chaiscript::ChaiScript chai;

    chai.add(chaiscript::user_type<Command>(), "Cmd");
    chai.add(chaiscript::fun(&Command::module), "module");
    chai.add(chaiscript::fun(&Command::store), "store");
    chai.add(chaiscript::fun(&Command::error), "error");
    chai.add(chaiscript::fun(&Command::link), "link");
    chai.add(chaiscript::fun(&Command::intern), "intern");
    chai.add(chaiscript::fun(&Command::val1), "val1");
    chai.add(chaiscript::fun(&Command::val2), "val2");
    chai.add(chaiscript::fun(&Command::val3), "val3");
    chai.add(chaiscript::fun(&Command::val4), "val4");
    chai.add(chaiscript::fun(&Command::var1), "var1");
    chai.add(chaiscript::fun(&Command::var2), "var2");
    chai.add(chaiscript::fun(&Command::var3), "var3");
    chai.add(chaiscript::fun(&Command::var4), "var4");
    chai.add(chaiscript::fun(&Command::var5), "var5");
    chai.add(chaiscript::fun(&Command::var6), "var6");
    chai.add(chaiscript::fun(&Command::var7), "var7");
    chai.add(chaiscript::fun(&Command::var8), "var8");
    chai.add(chaiscript::fun(&Command::var9), "var9");
    chai.add(chaiscript::fun(&Command::b1), "b1");
    chai.add(chaiscript::fun(&Command::b2), "b2");
    chai.add(chaiscript::fun(&Command::b3), "b3");

    std::map<std::string, chaiscript::Boxed_Value> functions;
    chai.add_global(chaiscript::var(functions), "f");

    chai.add(chaiscript::user_type<Fields>(), "Line");
    chai.add(chaiscript::fun(&Fields::getFieldValue), "get");

    try {
        int lineNr{0};

        for (auto line : lines) {
            ++lineNr;
            if (lineNr == 1) {
                // Prüfplan Kopfzeile
                continue;
            }
            struct Command cmd{0, 0, 0, 0 ,0 ,0 ,0, 0, 0, 0, 0 ,0 ,0 ,0, 0, 0, 0, 0 ,0 ,0 ,0};

            cmd.module = line.getBlockType();
            if (cmd.module == 0) {
                throw std::runtime_error("E: Test plan (" + std::to_string(lineNr)+"): Block type == 0!");
            }
            cmd.val1 = line.getVal1();

            chai.set_global(chaiscript::var(&line), "Line");
            chai.set_global(chaiscript::var(&cmd), "Cmd");

            try {
                std::string name = line.getBlockName();
                trim(name);
                chai.eval(chaiFunctions[name]);
                std::cout << "Cmd.b1 = " << cmd.b1 << std::endl;
            } catch (std::exception& e) {
                std::cerr << "Error in block definition: " << line.getBlockName() << std::endl;
                std::cerr << e.what() << std::endl;
                continue;
            }
        }
    } catch(const std::exception& e) {
        std::cerr << e.what() << std::endl;
    }

    return 0;
}
