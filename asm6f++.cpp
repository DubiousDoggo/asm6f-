
#include "asm6f++.hpp"
#include "nonstdlib.hpp"

#include <vector>
#include <iostream>
#include <algorithm>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include <stdarg.h>

static void *true_ptr = &true_ptr; // a pointer masquerading as a bool

unsigned current_pass = 0; // current assembly pass
unsigned current_scope;    // current scope, 0=global
unsigned nextscope;        // next nonglobal scope (increment on each new block of localized code)

bool lastchance = false; // set on final attempt
bool needanotherpass;    // still need to take care of some things..
bool error = false;      // hard error (stop assembly after this pass)
const char *errmsg;

char **makemacro = nullptr; // (during macro creation) where next macro line will go. set to true_ptr to skip past macro
char **makerept;            // like makemacro.. points to end of string chain
unsigned macrolevel = 0;    // number of nested macro/rept being expanded
unsigned reptcount = 0;     // counts rept statements during rept string storage
unsigned iflevel = 0;       // index into ifdone[],skipline[]
bool ifdone[IFNESTS];       // true if current IF level has been true
bool skipline[IFNESTS];     // true on an IF statement that is false

char *inputfilename = nullptr;     // input file name
char *outputfilename = nullptr;    // output file name
char *listfilename = nullptr;      // list file name
char *cdlfilename = nullptr;       // cdl file name
FILE *listfile = nullptr;          // list file pointer
FILE *outputfile = nullptr;        // output file pointer
FILE *cdlfile = nullptr;           // cdl file pointer
const char *includepath = nullptr; // include path for file resolution
const char *listerr = nullptr;     // error message for list file

// Command line options
bool verboselisting = false; // expand REPT loops in listing
bool genfceuxnl = false;     // generate FCEUX .nl files for symbolic debugging
bool genmesenlabels = false; // generate label files for use with Mesen
bool gencdl = false;         // generate CDL file
bool genlua = false;         // generate lua symbol file
bool verbose = true;         // output messages to stdout

bool nooutput = false;      // supress output (used with ENUM)
bool nonl = false;          // supress output to .nl files (used with IGNORENL)
bool allowunstable = false; // allow unstable instrucitons

// iNES header variables
bool ines_include = false;
int inesprg_num = 0;
int ineschr_num = 0;
int inesmir_num = 0;
int inesmap_num = 0;

int use_nes2 = 0;
int nes2chr_num = 0;
int nes2prg_num = 0;
int nes2sub_num = 0;
int nes2tv_num = 0;
int nes2vs_num = 0;
int nes2wram_num = 0;
int nes2bram_num = 0;
int nes2chrbram_num = 0;

byte outputbuff[BUFFSIZE];
byte inputbuff[BUFFSIZE];
int outcount; // bytes waiting in outputbuff

byte defaultfiller; // default fill value
int filepos = 0;

std::vector<comment *> comments;
int lastcommentpos = -1;
size_t commentcount;

std::vector<label *> labellist; // all of the labels allocated thus far
label *lastlabel;               // last label created
label *labelhere;               // points to the label being defined on the current line (for EQU, =, and MACRO)
label firstlabel{
    // '$' label
    "$",               // *name
    0,                 // value
    0,                 // pos
    (char *)&true_ptr, // *kitchen_sink
    VALUE,             // type
    false,             // used
    0,                 // pass
    0,                 // scope
    false,             // ignorenl
    nullptr,           // link
};

const std::vector<instruction> instructions{
    {"ADC", {{ABS, 0x6d}, {ABX, 0x7d}, {ABY, 0x79}, {IMM, 0x69}, {IDX, 0x61}, {IDY, 0x71}, {ZPX, 0x75}, {ZPG, 0x65}}},
    {"AND", {{ABS, 0x2d}, {ABX, 0x3d}, {ABY, 0x39}, {IMM, 0x29}, {IDX, 0x21}, {IDY, 0x31}, {ZPG, 0x25}, {ZPX, 0x35}}},
    {"ASL", {{ACC, 0x0a}, {ZPX, 0x16}, {ABX, 0x1e}, {ZPG, 0x06}, {ABS, 0x0e}, {IMP, 0x0a}}},
    {"BCC", {{REL, 0x90}}},
    {"BCS", {{REL, 0xb0}}},
    {"BEQ", {{REL, 0xf0}}},
    {"BIT", {{ABS, 0x2c}, {ZPG, 0x24}}},
    {"BMI", {{REL, 0x30}}},
    {"BNE", {{REL, 0xd0}}},
    {"BPL", {{REL, 0x10}}},
    {"BRK", {{IMM, 0x00}, {IMP, 0x00}, {ZPG, 0x00}}},
    {"BVC", {{REL, 0x50}}},
    {"BVS", {{REL, 0x70}}},
    {"CLC", {{IMP, 0x18}}},
    {"CLD", {{IMP, 0xd8}}},
    {"CLI", {{IMP, 0x58}}},
    {"CLV", {{IMP, 0xb8}}},
    {"CMP", {{ABS, 0xcd}, {ABX, 0xdd}, {ABY, 0xd9}, {IMM, 0xc9}, {IDX, 0xc1}, {IDY, 0xd1}, {ZPG, 0xc5}, {ZPX, 0xd5}}},
    {"CPX", {{ABS, 0xec}, {IMM, 0xe0}, {ZPG, 0xe4}}},
    {"CPY", {{ABS, 0xcc}, {IMM, 0xc0}, {ZPG, 0xc4}}},
    {"DEC", {{ABS, 0xce}, {ABX, 0xde}, {ZPG, 0xc6}, {ZPX, 0xd6}}},
    {"DEX", {{IMP, 0xca}}},
    {"DEY", {{IMP, 0x88}}},
    {"EOR", {{ABS, 0x4d}, {ABY, 0x59}, {ABX, 0x5d}, {IMM, 0x49}, {IDX, 0x41}, {IDY, 0x51}, {ZPG, 0x45}, {ZPX, 0x55}}},
    {"INC", {{ABS, 0xee}, {ABX, 0xfe}, {ZPG, 0xe6}, {ZPX, 0xf6}}},
    {"INX", {{IMP, 0xe8}}},
    {"INY", {{IMP, 0xc8}}},
    {"JMP", {{ABS, 0x4c}, {IND, 0x6c}}},
    {"JSR", {{ABS, 0x20}}},
    {"LDA", {{ABS, 0xad}, {ABX, 0xbd}, {ABY, 0xb9}, {IMM, 0xa9}, {IDX, 0xa1}, {IDY, 0xb1}, {ZPG, 0xa5}, {ZPX, 0xb5}}},
    {"LDX", {{ABS, 0xae}, {ABY, 0xbe}, {IMM, 0xa2}, {ZPG, 0xa6}, {ZPY, 0xb6}}},
    {"LDY", {{ABS, 0xac}, {ABX, 0xbc}, {IMM, 0xa0}, {ZPG, 0xa4}, {ZPX, 0xb4}}},
    {"LSR", {{ACC, 0x4a}, {ABS, 0x4e}, {ABX, 0x5e}, {IMP, 0x4a}, {ZPG, 0x46}, {ZPX, 0x56}}},
    {"NOP", {{IMP, 0xea}}},
    {"ORA", {{ABS, 0x0d}, {ABX, 0x1d}, {ABY, 0x19}, {IMM, 0x09}, {IDX, 0x01}, {IDY, 0x11}, {ZPG, 0x05}, {ZPX, 0x15}}},
    {"PHA", {{IMP, 0x48}}},
    {"PHP", {{IMP, 0x08}}},
    {"PLA", {{IMP, 0x68}}},
    {"PLP", {{IMP, 0x28}}},
    {"ROL", {{ACC, 0x2a}, {ABS, 0x2e}, {ABX, 0x3e}, {IMP, 0x2a}, {ZPG, 0x26}, {ZPX, 0x36}}},
    {"ROR", {{ACC, 0x6a}, {ABS, 0x6e}, {ABX, 0x7e}, {IMP, 0x6a}, {ZPG, 0x66}, {ZPX, 0x76}}},
    {"RTI", {{IMP, 0x40}}},
    {"RTS", {{IMP, 0x60}}},
    {"SBC", {{ABS, 0xed}, {ABX, 0xfd}, {ABY, 0xf9}, {IMM, 0xe9}, {IDX, 0xe1}, {IDY, 0xf1}, {ZPG, 0xe5}, {ZPX, 0xf5}}},
    {"SEC", {{IMP, 0x38}}},
    {"SED", {{IMP, 0xf8}}},
    {"SEI", {{IMP, 0x78}}},
    {"STA", {{ABS, 0x8d}, {ABX, 0x9d}, {ABY, 0x99}, {IDX, 0x81}, {IDY, 0x91}, {ZPG, 0x85}, {ZPX, 0x95}}},
    {"STX", {{ABS, 0x8e}, {ZPG, 0x86}, {ZPY, 0x96}}},
    {"STY", {{ABS, 0x8c}, {ZPG, 0x84}, {ZPX, 0x94}}},
    {"TAX", {{IMP, 0xaa}}},
    {"TAY", {{IMP, 0xa8}}},
    {"TSX", {{IMP, 0xba}}},
    {"TXA", {{IMP, 0x8a}}},
    {"TXS", {{IMP, 0x9a}}},
    {"TYA", {{IMP, 0x98}}},

    /* undocumented opcodes */
    {"ALR", {{IMM, 0x4b}}},
    {"ANC", {{IMM, 0x0b}}},
    {"ARR", {{IMM, 0x6b}}},
    {"AXS", {{IMM, 0xcb}}},
    {"DCP", {{ABS, 0xcf}, {ABX, 0xdf}, {ABY, 0xdb}, {IDX, 0xc3}, {IDY, 0xd3}, {ZPG, 0xc7}, {ZPX, 0xd7}}},
    {"ISC", {{ABS, 0xef}, {ABX, 0xff}, {ABY, 0xfb}, {IDX, 0xe3}, {IDY, 0xf3}, {ZPG, 0xe7}, {ZPX, 0xf7}}},
    {"LAS", {{ABY, 0xbb}}},
    {"LAX", {{ZPG, 0xa7}, {ZPY, 0xb7}, {IDX, 0xa3}, {IDY, 0xb3}, {ABS, 0xaf}, {ABY, 0xbf}}},
    {"RLA", {{ABS, 0x2f}, {ABX, 0x3f}, {ABY, 0x3b}, {IDX, 0x23}, {IDY, 0x33}, {ZPG, 0x27}, {ZPX, 0x37}}},
    {"RRA", {{ABS, 0x6f}, {ABX, 0x7f}, {ABY, 0x7b}, {IDX, 0x63}, {IDY, 0x73}, {ZPG, 0x67}, {ZPX, 0x77}}},
    {"SAX", {{ABS, 0x8f}, {IDX, 0x83}, {ZPG, 0x87}, {ZPY, 0x97}}},
    {"SLO", {{ABS, 0x0f}, {ABX, 0x1f}, {ABY, 0x1b}, {IDX, 0x03}, {IDY, 0x13}, {ZPG, 0x07}, {ZPX, 0x17}}},
    {"SRE", {{ABS, 0x4f}, {ABX, 0x5f}, {ABY, 0x5b}, {IDX, 0x43}, {IDY, 0x53}, {ZPG, 0x47}, {ZPX, 0x57}}},

    /* unstable opcodes */
    {"AHX", {{ABY, 0x9f}, {IDY, 0x93}}},
    {"SHX", {{ABY, 0x9e}}},
    {"SHY", {{ABX, 0x9c}}},
    {"TAS", {{ABY, 0x9b}}},
    {"XAA", {{IMM, 0x8b}}},
};

const char *unstablelist[] = {
    "AHX", "SHY", "SHX", "TAS", "XAA"};

const std::vector<directive> directives{
    {"", nothing},
    {"IF", if_},
    {"ELSEIF", elseif},
    {"ELSE", else_},
    {"ENDIF", endif},
    {"IFDEF", ifdef},
    {"IFNDEF", ifndef},
    {"=", equal},
    {"EQU", equ},
    {"ORG", org},
    {"BASE", base},
    {"PAD", pad},
    {"INCLUDE", include},
    {"INCSRC", include},
    {"INCBIN", incbin},
    {"BIN", incbin},
    {"HEX", hex},
    {"WORD", dw},
    {"DW", dw},
    {"DCW", dw},
    {"DC.W", dw},
    {"BYTE", db},
    {"DB", db},
    {"DCB", db},
    {"DC.B", db},
    {"DSW", dsw},
    {"DS.W", dsw},
    {"DSB", dsb},
    {"DS.B", dsb},
    {"ALIGN", align},
    {"MACRO", macro},
    {"REPT", rept},
    {"ENDM", endm},
    {"ENDR", endr},
    {"ENUM", enum_},
    {"ENDE", ende},
    {"IGNORENL", ignorenl},
    {"ENDINL", endinl},
    {"FILLVALUE", fillval},
    {"DL", dl},
    {"DH", dh},
    {"ERROR", make_error},
    {"INESPRG", inesprg},
    {"INESCHR", ineschr},
    {"INESMIR", inesmir},
    {"INESMAP", inesmap},
    {"NES2CHRRAM", nes2chrram},
    {"NES2PRGRAM", nes2prgram},
    {"NES2SUB", nes2sub},
    {"NES2TV", nes2tv},
    {"NES2VS", nes2vs},
    {"NES2BRAM", nes2bram},
    {"NES2CHRBRAM", nes2chrbram},
    {"UNSTABLE", unstable}};

const char OutOfRange[] = "Value out of range.";
const char SeekOutOfRange[] = "Seek position out of range.";
const char BadIncbinSize[] = "INCBIN size is out of range.";
const char NotANumber[] = "Not a number.";
const char UnknownLabel[] = "Unknown label.";
const char Illegal[] = "Illegal instruction.";
const char IncompleteExp[] = "Incomplete expression.";
const char LabelDefined[] = "Label already defined.";
const char MissingOperand[] = "Missing operand.";
const char DivZero[] = "Divide by zero.";
const char BadAddr[] = "Can't determine address.";
const char NeedName[] = "Need a name.";
const char CantOpen[] = "Can't open file.";
const char ExtraENDM[] = "ENDM without MACRO.";
const char ExtraENDR[] = "ENDR without REPT.";
const char ExtraENDE[] = "ENDE without ENUM.";
const char ExtraENDINL[] = "ENDINL without IGNORENL.";
const char RecurseMACRO[] = "Recursive MACRO not allowed.";
const char RecurseEQU[] = "Recursive EQU not allowed.";
const char NoENDIF[] = "Missing ENDIF.";
const char NoENDM[] = "Missing ENDM.";
const char NoENDR[] = "Missing ENDR.";
const char NoENDE[] = "Missing ENDE.";
const char NoENDINL[] = "Missing ENDINL.";
const char IfNestLimit[] = "Too many nested IFs.";
const char UndefinedPC[] = "PC is undefined (use ORG first)";

const std::string whitespace_filename{"\t\r\n\""};
#define whitesp2 whitespace_filename.c_str() // (used for filename processing)

const std::string whitespace{" \t\r\n"};
#define whitesp whitespace.c_str()

const std::string math_chars{"!^&|+-*/%()<>=,"};
#define mathy math_chars.c_str()

const std::string math_delimiters{whitespace + math_chars};

char tmpstr[LINEMAX]; // all purpose big string

// Prints printf-style message to stderr, then exits.
// Closes and deletes output file.
static void fatal_error(const char fmt[], ...)
{
  va_list args;

  if (outputfile != nullptr)
  {
    fclose(outputfile);
    remove(outputfilename);
  }

  va_start(args, fmt);
  fprintf(stderr, "\nError: ");
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n\n");
  va_end(args);

  exit(EXIT_FAILURE);
}

// Prints printf-style message if verbose mode is enabled.
static void message(const char fmt[], ...)
{
  if (verbose)
  {
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
  }
}

// Same as malloc(), but prints error and exits if allocation fails
static char *my_malloc(size_t s)
{
  char *p = static_cast<char *>(malloc(s ? s : 1));
  if (p == nullptr)
    fatal_error("out of memory");

  return p;
}

// Same as common strdup(), but prints error and exits if allocation fails
static char *my_strdup(const char *in)
{
  size_t size = strlen(in) + 1;
  char *out = my_malloc(size);
  memcpy(out, in, size);
  return out;
}

// -------------------------------------------------------
// parsing functions
// -------------------------------------------------------

// Not all systems support this, so we implement our own always.
// More problematic to try to use the system's version rather than
// ours in all cases.
char *my_strupr(char *string)
{

  if (string == nullptr)
  {
    return nullptr;
  }

  for (char *s = string; *s != '\0'; s++)
  {
    *s = toupper((unsigned char)*s);
  }

  return string;
}

byte from_hex(const char i)
{
  if (i >= '0' && i <= '9')
    return i - '0';
  else if (i >= 'a' && i <= 'f')
    return i - ('a' - 10);
  else if (i >= 'A' && i <= 'F')
    return i - ('A' - 10);

  errmsg = NotANumber;
  return -1;
}

byte from_bin(const char i)
{
  if (i == '0')
    return 0;
  if (i == '1')
    return 1;

  errmsg = NotANumber;
  return -1;
}

// decode str into a number
// set errmsg on error
bool dependant; // set to nonzero if symbol couldn't be resolved
int getvalue(std::string::const_iterator &begin, const std::string::const_iterator &end)
{
  const std::string gv = nonstd::getword(begin, end, whitespace, math_delimiters);
  if (gv.empty())
  {
    errmsg = MissingOperand;
    return 0;
  }

  const char prefix = std::tolower(gv.front());
  const char suffix = std::tolower(gv.back());

  if (prefix == '$' && gv.length() == 1)
    return addr; // $ by itself is the PC

  auto gv_begin = gv.begin();
  auto gv_end = gv.end();

  if (prefix == '$' || (std::isdigit(prefix) && suffix == 'h'))
  { // hexadecimal
    if ((prefix == '$' && suffix == 'h') || (prefix == suffix))
    { // dont allow duplicate specifiers or standalone $/h
      errmsg = NotANumber;
      return 0;
    }

    if (prefix == '$')
      gv_begin++;
    if (suffix == 'h')
      gv_end--;

    unsigned value = 0;
    unsigned chars = 0;
    while (gv_begin != gv_end)
    {
      value = (value << 4) | from_hex(*gv_begin);
      chars++;
      gv_begin++;
    }
    if (chars > 8)
      errmsg = OutOfRange;
    return value;
  }

  if (prefix == '%' || (std::isdigit(prefix) && suffix == 'b'))
  { // binary
    if ((prefix == '%' && suffix == 'b') || (prefix == suffix))
    { // dont allow duplicate specifiers or standalone %/b
      errmsg = NotANumber;
      return 0;
    }

    if (prefix == '%')
      gv_begin++;
    if (suffix == 'b')
      gv_end--;

    unsigned value = 0;
    unsigned chars = 0;
    while (gv_begin != gv_end)
    {
      value = (value << 1) | from_bin(*gv_begin);
      chars++;
      gv_begin++;
    }
    if (chars > 32)
      errmsg = OutOfRange;
    return value;
  }

  if (prefix == '\'' || prefix == '"')
  { // char
    gv_begin++;
    if (*gv_begin == '\\')
      gv_begin++; // escaped char
    int ret = *gv_begin;
    gv_begin++;
    if (gv_begin != gv_end || *gv_begin != prefix)
      errmsg = NotANumber;
    return ret;
  }

  if (std::isdigit(prefix))
  { // number
    try
    {
      return std::stoi(std::string(gv_begin, gv_end), nullptr, 10);
    }
    catch (const std::out_of_range &)
    {
      errmsg = OutOfRange;
      return 0;
    }
    catch (const std::invalid_argument &)
    {
      errmsg = NotANumber;
      return 0;
    }
  }

  { // label
    label *p = findlabel(gv);
    if (p == nullptr)
    { // label doesn't exist (yet?)
      needanotherpass = true;
      dependant = 1;
      if (lastchance)
      { // only show error once we're certain label will never exist
        errmsg = UnknownLabel;
      }
      return 0;
    }
    else
    {
      dependant |= !p->kitchen_sink;
      needanotherpass |= !p->kitchen_sink;
      if (p->type == LABEL || p->type == VALUE)
      {
        return p->value;
      }
      else if (p->type == MACRO)
      {
        errmsg = "Can't use macro in expression.";
        return 0;
      }
      else
      { // what else is there?
        errmsg = UnknownLabel;
        return 0;
      }
    }
  }
}

// get operator from str and advance str
operators getoperator(std::string::const_iterator &str, const std::string::const_iterator &end)
{
  str = nonstd::eat_characters(str, end, whitespace);
  str++;
  switch (*(str - 1))
  {
  case '&':
    if (*str == '&')
    {
      str++;
      return ANDAND;
    }
    else
      return AND;
  case '|':
    if (*str == '|')
    {
      str++;
      return OROR;
    }
    else
      return OR;
  case '^':
    return XOR;
  case '+':
    return PLUS;
  case '-':
    return MINUS;
  case '*':
    return MUL;
  case '%':
    return MOD;
  case '/':
    return DIV;
  case '=':
    if (*str == '=')
      str++;
    return EQUAL;
  case '>':
    if (*str == '=')
    {
      str++;
      return GREATEREQ;
    }
    else if (*str == '>')
    {
      str++;
      return RIGHTSHIFT;
    }
    else
      return GREATER;
  case '<':
    if (*str == '=')
    {
      str++;
      return LESSEQ;
    }
    else if (*str == '>')
    {
      str++;
      return NOTEQUAL;
    }
    else if (*str == '<')
    {
      str++;
      return LEFTSHIFT;
    }
    else
      return LESS;
  case '!':
    if (*str == '=')
    {
      str++;
      return NOTEQUAL;
    }
    // no break
  default:
    str--;
    return NOOP;
  }
}

// evaluate expression in str and advance str
int eval(std::string::const_iterator &str, const std::string::const_iterator &end, const prectypes precedence)
{
  int ret, val2;
  auto s = nonstd::eat_characters(str, end, whitespace);
  const char unary = *s;
  switch (unary)
  {
  case '(':
    s++;
    ret = eval(s, end, WHOLEEXP);
    s = nonstd::eat_characters(s, end, whitespace);
    if (*s == ')')
      s++;
    else
      errmsg = IncompleteExp;
    break;
  case '#':
    s++;
    ret = eval(s, end, WHOLEEXP);
    break;
  case '~':
    s++;
    ret = ~eval(s, end, UNARY);
    break;
  case '!':
    s++;
    ret = !eval(s, end, UNARY);
    break;
  case '<':
    s++;
    ret = eval(s, end, UNARY) & 0xff;
    break;
  case '>':
    s++;
    ret = (eval(s, end, UNARY) >> 8) & 0xff;
    break;
  case '+':
  case '-':
    // careful.. might be a +-label
    { // new scope here for variable allocation
      auto s2 = s;
      s++;
      int dependant_temp = dependant; // eval() is recursive so don't mess up dependant
      bool another_pass_temp = needanotherpass;
      dependant = 0;
      ret = getvalue(s2, end);
      if (errmsg == UnknownLabel)
        errmsg = nullptr;

      if (!dependant || s2 == s)
      { // found a label or single +/-
        s = s2;
        s2 = end;
        dependant |= dependant_temp;
      }
      else
      { // not a label after all..
        dependant = dependant_temp;
        needanotherpass = another_pass_temp;
      }
      if (s2 != end)
      { // if it wasnt a +/- label
        ret = eval(s, end, UNARY);
        if (unary == '-')
          ret = -ret;
      }
    }
    break;
  default:
    ret = getvalue(s, end);
  }

  operators op;
  do
  {
    str = s;
    op = getoperator(s, end);
    if (prec[op] > precedence)
    {
      val2 = eval(s, end, prec[op]);
      if (dependant)
      {
        ret = 0;
      }
      else
      {
        switch (op)
        {
        case AND:
          ret &= val2;
          break;
        case ANDAND:
          ret = ret && val2;
          break;
        case OR:
          ret |= val2;
          break;
        case OROR:
          ret = ret || val2;
          break;
        case XOR:
          ret ^= val2;
          break;
        case PLUS:
          ret += val2;
          break;
        case MINUS:
          ret -= val2;
          break;
        case MUL:
          ret *= val2;
          break;
        case DIV:
          if (val2 == 0)
            errmsg = DivZero;
          else
            ret /= val2;
          break;
        case MOD:
          if (val2 == 0)
            errmsg = DivZero;
          else
            ret %= val2;
          break;
        case EQUAL:
          ret = (ret == val2);
          break;
        case NOTEQUAL:
          ret = (ret != val2);
          break;
        case GREATER:
          ret = ret > val2;
          break;
        case GREATEREQ:
          ret = ret >= val2;
          break;
        case LESS:
          ret = ret < val2;
          break;
        case LESSEQ:
          ret = ret <= val2;
          break;
        case LEFTSHIFT:
          ret <<= val2;
          break;
        case RIGHTSHIFT:
          ret >>= val2;
          break;
        case NOOP:
          break;
        }
      }
    }
  } while (prec[op] > precedence && !errmsg);
  return ret;
}

FILE *getfile(std::string filename, const char *args)
{
  filename.erase(0, filename.find_first_not_of(" \t\r\n\""));
  filename.erase(filename.find_last_not_of(" \t\r\n\"") + 1);
  FILE *f = fopen(filename.c_str(), args);
  if (!f && includepath != nullptr)
  { // try with path
    filename.insert(0, "\\");
    filename.insert(0, includepath);
    f = fopen(filename.c_str(), args);
  }
  if (!f)
  {
    return nullptr;
  }
  return f;
}

// get word in src, advance src, and return label*
// if the word is not a valid label, or if a label was found that is not a
// macro or reserved word, sets errmsg and returns nullptr .
label *getreserved(std::string::const_iterator &src, const std::string::const_iterator &end)
{
  std::string dst;
  std::string upp;

  src = nonstd::eat_characters(src, end, whitespace);
  if (*src == '=')
  { // special '=' reserved word
    upp = "=";
    src++;
  }
  else
  {
    if (*src == '.') // reserved words can start with "."
      src++;
    dst = nonstd::getword(src, end, whitespace);
    upp = nonstd::to_upper(dst);
  }

  label *p = findlabel(upp); // case insensitive reserved word
  if (p == nullptr)
    p = findlabel(dst); // or case sensitive macro

  if (p != nullptr)
  {
    if (p->type == MACRO)
    { // macros can only be used after they are defined
      if (p->pass != current_pass)
        p = nullptr;
    }
    else if (p->type != RESERVED)
    {
      p = nullptr;
    }
  }

  if (p == nullptr)
    errmsg = Illegal;

  return p;
}

// Copies next word to dst and advances next.
// Returns true if the word is a valid label, otherwise sets error and returns false.
// If dst has a trailing `:` it will be removed.
// The special `$` label holds the current program address. Labels beginning
// with `@` are local labels. They have limited scope, visible only between
// non-local labels. Names of local labels may be reused. Labels beginning with
// one or more `+` or `-` characters are nameless labels. Forward labels
// (beginning with `+`) can only be used before their labeled location.
// Backwards labels can only be used after their labeled location. Nameless
// labels can be reused, they will always refer to the label closest to the
// current line. Label names must start with either an alphabetic character or
// an underscore to be considered valid.
bool good_label(std::string &dst, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  // TODO should it set error?
  dst = nonstd::getword(next, end, whitespace, math_delimiters);
  if (dst.empty())
  {
    errmsg = Illegal;
    return false;
  }

  if (dst.back() == ':')
    dst.pop_back();

  if (dst.length() == 1 && dst[0] == '$') // '$' label
    return true;

  auto s = dst.begin();
  char first = *s;
  if (first == '+' || first == '-')
  { // nameless label
    do
      s++;
    while (*s == first);
    if (s == dst.end()) // just ++.. or --.., no text
      return true;
    first = *s;
  }
  if (first == LOCALCHAR || first == '_' || std::isalpha(first))
  { // label can start with these
    return true;
  }
  else
  {
    errmsg = Illegal; // bad label
    return false;
  }
}

// Expands equates from src into dst, excluding comment
// src is advanced to the begining of the comment if there is one, or end
void expandline(std::string &dst, std::string::const_iterator &src, const std::string::const_iterator &end)
{ // TODO document examples
  bool def_skip = false;
  while (src != end)
  {
    char c = *src;
    if (c == '$' || std::isdigit(c))
    { // read past digits (could be mistaken for a symbol, i.e. $BEEF)
      do
      {
        dst += c;
        src++;
        c = *src;
      } while (src != end && (std::isxdigit(c) || std::tolower(c) == 'h'));
      continue;
    }
    else if (c == '"' || c == '\'')
    { // read past quotes
      do
      {
        dst += *src;
        if (*src == '\\')
        {
          src++;
          dst += *src;
        }
        src++;
      } while (src != end && *src != c);
      dst += c;
    }
    else if (c == '_' || c == '.' || c == LOCALCHAR || std::isalpha(c))
    { // symbol
      auto start = src;
      do
      { // scan to end of symbol
        src++;
        c = *src;
      } while (c == '_' || c == '.' || c == LOCALCHAR || std::isalnum(c));

      /*
      ghey hack.
      expandline() is called quite early during parsing, so

      FOO equ xxxx
      ifdef FOO
        
        becomes

      FOO equ xxxx
      ifdef xxxx

      rendering IFDEF useless, so we will bypass expansion in this special case.
      I'm avoiding getreserved() because it does searching and other unnecessary crap.
      */

      label *p = nullptr;
      if (!def_skip)
      {
        const std::string temp{start + (*start == '.'), src};
        const std::string temp_upper = nonstd::to_upper(temp);
        if ((temp_upper == "IFDEF") ||
            (temp_upper == "IFNDEF"))
          def_skip = true;
        else
          p = findlabel(temp);
      }
      if (p != nullptr)
      {
        if (p->type != EQUATE || p->pass != current_pass)
        { // equates MUST be defined before being used otherwise they will be expanded in their own definition
          // i.e. (label equ whatever) gets translated to (whatever equ whatever)
          p = nullptr;
        }
        else if (p->used)
        {
          p = nullptr;
          errmsg = RecurseEQU;
          break;
        }
      }
      if (p != nullptr)
      { // p is a valid equate
        p->used = true;
        const std::string temp{p->kitchen_sink};
        auto temp_iter = temp.begin();
        expandline(dst, temp_iter, temp.end());
        p->used = false;
      }
      else
      {
        dst.append(start, src);
      }
    }
    else
    {
      if (c == ';')
      { // comment
        return;
      }
      dst += c;
      src++;
    }
  }
}

bool next_arg(std::string::const_iterator &str, const std::string::const_iterator &end)
{
  str = nonstd::eat_characters(str, end, whitespace);
  if (str != end && *str == ',')
  {
    str++;
    return true;
  }
  return false;
}

void export_labelfiles()
{
  // iterate through all the labels and output FCEUX-compatible label info files
  // based on their type (LABEL's,EQUATE's,VALUE's), address (ram/rom), and position (bank)
  char str[512];
  char filename[512];
  FILE *bankfiles[64];
  char *strptr;

  for (int i = 0; i < 64; i++)
  {
    bankfiles[i] = 0;
  }

  // ram file: <output>.ram.nl
  // bank files: <output>.<bank number in hex>.nl

  strcpy(filename, outputfilename);

  strptr = strrchr(filename, '.'); // strptr ='.'ptr
  if (strptr)
    if (strchr(strptr, '\\'))
      strptr = 0; // watch out for "dirname.ext\listfile"
  if (!strptr)
    strptr = filename + strlen(str); // strptr -> inputfile extension
  strcpy(strptr, ".nes.ram.nl");

  FILE *ramfile = fopen(filename, "w");

  // the bank files are created ad-hoc before being written to.

  // iNES banks are 16kb. Subtracting the 16 byte header and dividing that by
  // 16384 will get us the bank number of a particular label.
  // this only applies to labels that are $8000 and up.
  // Anything below $8000 is assumed to be RAM.

  // todo: include EQUATES for other registers and variables

  for (const label *l : labellist)
  {

    // [freem addition]: handle IGNORENL'd labels
    if (l->ignorenl)
      continue;

    if (
        (
            l->type == LABEL ||
            ((l->type == EQUATE || l->type == VALUE) && l->name.length() > 1)) &&
        l->value < 0x10000)
    {
      sprintf(str, "$%04X#%s#\n", (unsigned int)l->value, l->name.c_str());
      // puts(str);

      if (l->value < 0x8000)
      { // RAM
        fwrite((const void *)str, 1, strlen(str), ramfile);
      }
      else
      { // ROM
        int bank = ((l->pos - 16) / 16384);
        if (!bankfiles[bank])
        {
          sprintf(strptr, ".nes.%X.nl", bank);
          bankfiles[bank] = fopen(filename, "w");
        }
        fwrite((const void *)str, 1, strlen(str), bankfiles[bank]);
      }
    }
  }

  fclose(ramfile);

  for (int i = 0; i < 64; i++)
  {
    if (bankfiles[i])
      fclose(bankfiles[i]);
  }
}

// iterate through all the labels and output Lua-compatible label info files
void export_lua()
{
  char str[512];
  char filename[512];
  FILE *mainfile;
  char *strptr;
  strcpy(filename, outputfilename);

  strptr = strrchr(filename, '.'); // strptr ='.'ptr
  if (strptr)
    if (strchr(strptr, '\\'))
      strptr = 0; // watch out for "dirname.ext\listfile"
  if (!strptr)
    strptr = filename + strlen(str); // strptr -> inputfile extension
  strcpy(strptr, ".lua");

  mainfile = fopen(filename, "w");

  for (const label *l : labellist)
  {

    if ((l->type == LABEL || ((l->type == EQUATE || l->type == VALUE) && l->name.length() > 1)) &&
        l->name[0] != '-' && l->name[0] != '+') // no anonymous labels
    {
      sprintf(str, "%s = 0x%04X\n", l->name.c_str(), (unsigned int)l->value);
      fwrite((const void *)str, 1, strlen(str), mainfile);
    }
  }

  fclose(mainfile);
}

bool comparelabels(const label *a, const label *b)
{
  if (a->type < b->type)
    return true;
  if (a->type > b->type)
    return false;
  if (a->pos < b->pos)
    return true;
  if (a->pos > b->pos)
    return false;
  if (a->value < b->value)
    return true;
  if (a->value > b->value)
    return false;
  return a->name < b->name;
}

int comparecomments(const void *arg1, const void *arg2)
{
  const comment *a = *((comment **)arg1);
  const comment *b = *((comment **)arg2);
  if (a->pos > b->pos)
    return 1;
  if (a->pos < b->pos)
    return -1;
  return strcmp(a->text.c_str(), b->text.c_str());
}

// iterate through all the labels and output Mesen-compatible label files
// based on their type (LABEL's,EQUATE's,VALUE's) and address (ram/rom)
void export_mesenlabels()
{
  std::string filename{outputfilename};

  // Strip the extension from output filename
  size_t extension = filename.rfind('.');
  if (extension != std::string::npos)
    if (filename.find('\\', extension) != std::string::npos) // watch out for "dir.ext\file"
      extension = std::string::npos;

  if (extension == std::string::npos)
    extension = filename.size();

  filename.replace(extension, std::string::npos, ".mlb");

  FILE *outfile = fopen(filename.c_str(), "w");

  std::sort(labellist.begin(), labellist.end(), &comparelabels); // do these need to be sorted?
  std::sort(comments.begin(), comments.end(), comparecomments);

  char str[512];
  size_t currentcomment = 0;
  for (const label *l : labellist)
  {
    if (l->type != LABEL && l->type != VALUE && l->type != EQUATE)
    { // Ignore macros and reserved words
      continue;
    }
    if (l->value >= 0x10000 || l->value < 0 || l->name[0] == '+' || l->name[0] == '-')
    { // Ignore CHR & anonymous code labels
      continue;
    }

    if (l->type == LABEL)
    {                  // Labels in the actual code
      if (l->pos < 16) // TODO - this thows out labels, pos is messed up?
      {                // Ignore file header
        continue;
      }

      //Check if one or more comments match this address
      const char *commenttext = nullptr;
      while (currentcomment < commentcount)
      {
        comment *c = comments[currentcomment];

        if (c->pos < l->pos)
        {
          // This comment is for a line before the current code label, write it to the file right away
          if (c->pos >= 16)
          {
            sprintf(str, "P:%04X::", (unsigned int)c->pos - 16);
            fwrite((const void *)str, 1, strlen(str), outfile);
            fwrite((const void *)c->text.c_str(), 1, strlen(c->text.c_str()), outfile);
            fwrite("\n", 1, 1, outfile);
          }
          currentcomment++;
        }
        else if (c->pos == l->pos)
        {
          //Same address, write it on the same line as the label
          commenttext = c->text.c_str();
          currentcomment++;
          break;
        }
        else
        {
          break;
        }
      }

      // Dump the label
      sprintf(str, "P:%04X:%s", (unsigned int)(l->pos - 16), l->name.c_str());
      fwrite((const void *)str, 1, strlen(str), outfile);

      if (commenttext)
      {
        fwrite(":", 1, 1, outfile);
        fwrite((const void *)commenttext, 1, strlen(commenttext), outfile);
      }
      fwrite("\n", 1, 1, outfile);
    }
    else if (l->type == VALUE || l->type == EQUATE)
    {
      // These are potentially aliases for variables in RAM, or read/write registers, etc.
      if (l->value < 0x2000)
      {
        // TODO - dont assume
        // Assume nes internal RAM below $2000 (2kb)
        sprintf(str, "R:%04X:%s\n", (unsigned int)l->value, l->name.c_str());
      }
      else if (l->value >= 0x6000 && l->value < 0x8000)
      {
        // Assume save/work RAM ($6000-$7FFF), dump as both. (not the best solution - maybe an option?)
        sprintf(str, "S:%04X:%s\n", (unsigned int)l->value - 0x6000, l->name.c_str());
        sprintf(str, "W:%04X:%s\n", (unsigned int)l->value - 0x6000, l->name.c_str());
      }
      else
      {
        // Assume a global register for everything else (e.g $8000 for mapper control, etc.)
        sprintf(str, "G:%04X:%s\n", (unsigned int)l->value, l->name.c_str());
      }
      fwrite((const void *)str, 1, strlen(str), outfile);
    }
  }

  fclose(outfile);
}

// Creates a new label and adds it to the label list. Sets labelhere to the new label.
// If force_local is set, the label will be treated as a local, otherwise the
// label will be local only if it starts with LOCALCHAR.
void addlabel(std::string label_name, bool force_local)
{
  const bool local = force_local || label_name.at(0) == LOCALCHAR;

  label *p = findlabel(label_name); // check if theres an existing label with the same name
  if (p != nullptr && p->scope == 0 && p->type != VALUE && local)
    p = nullptr; // if it's global and we're local, pretend we didn't see it (local label overrides global of the same name)

  // global labels advance current_scope
  if (!local)
  {
    current_scope = nextscope++;
  }

  if (p == nullptr)
  { // new label
    labelhere = newlabel(label_name);
    labelhere->type = LABEL; // assume it's a label.. could mutate into something else later
    labelhere->pass = current_pass;
    labelhere->value = addr;
    labelhere->kitchen_sink = reinterpret_cast<char *>(addr >= 0 ? true_ptr : nullptr);
    labelhere->used = false;
    labelhere->pos = filepos;
    labelhere->ignorenl = nonl;

    if (local)
    { // local
      labelhere->scope = current_scope;
    }
    else
    { // global
      labelhere->scope = 0;
    }
    lastlabel = labelhere;
  }
  else
  { // old label
    labelhere = p;
    if (p->pass == current_pass && label_name[0] != '-')
    { // if this label already encountered on this pass and not a backwards label
      if (p->type == VALUE)
        return; // values can be redefined
      else
        errmsg = LabelDefined;
    }
    else
    { // first time seen on this pass or a backwards label
      p->pass = current_pass;
      if (p->type == LABEL)
      {
        if (p->value != addr && label_name[0] != '-')
        { // label position is still moving around
          needanotherpass = true;
          if (lastchance)
            errmsg = BadAddr;
        }
        p->value = addr;
        p->pos = filepos;
        p->kitchen_sink = reinterpret_cast<char *>(addr >= 0 ? true_ptr : nullptr);
        if (lastchance && addr < 0)
          errmsg = BadAddr;
      }
    }
  }
}

// initialize label list
void initlabels()
{
  label *p;

  labellist = std::vector<label *>();
  labellist.push_back(&firstlabel); // '$' label

  // add reserved words to label list
  for (const auto &instruction : instructions)
  { // opcodes first
    p = newlabel(instruction.mnemonic.c_str());
    p->value = (ptrdiff_t)opcode;
    p->kitchen_sink = const_cast<char *>(reinterpret_cast<const char *>(&instruction.opcodes));
    p->type = RESERVED;
  }

  for (const auto &directive : directives)
  { // other reserved words now
    p = newlabel(directive.name);
    p->value = (ptrdiff_t)directive.func;
    p->type = RESERVED;
  }
  lastlabel = p;
}

void addcomment(const std::string &text)
{
  static unsigned oldpass = 0;
  int commentcount = comments.size();
  if (oldpass != current_pass)
  {
    oldpass = current_pass;
    commentcount = 0;
  }

  auto text_begin = text.cbegin();
  auto text_end = text.cend();
  if (*text_begin == ';')
    text_begin++; // ignore leading `;`
  if (*text_end == '\n')
    text_end--; // ignore trailing newline

  if (lastcommentpos == filepos)
  { // Append comment to the previous comment, since they are for the same address
    comment *c = comments.at(commentcount - 1);
    c->text.append(text_begin, text_end);
  }
  else
  { // Add a new comment
    comment *c = new comment;
    c->pos = filepos;
    c->text = std::string(text_begin, text_end);

    comments.at(commentcount) = c;
    commentcount++;

    lastcommentpos = filepos;
  }
}

// finds a label label with this label_name.
// returns a pointer to the label if a label with the correct name and scope was
// found, otherwise nullptr.
label *findlabel(std::string label_name)
{
  // find points to the first element in label list that whose name is not less than label name.
  // i.e. the first element where `(*find)->name >= label_name`
  auto find = std::lower_bound(labellist.begin(), labellist.end(), label_name,
                               [](label *const &a, const std::string &b) {
                                 return a->name < b;
                               });

  if (find == labellist.end() || label_name.compare((*find)->name) != 0)
  { // label was not found
    return nullptr;
  }

  // check scope: p is only visible if p's scope is the current scope or p's
  // scope is global
  label *p = *find;
  label *global = nullptr;
  if (!label_name.empty() && label_name.at(0) == '+')
  { // forward labels need special treatment
    do
    {
      if (p->pass != current_pass)
      { // dont consider forward labels already processed on this pass
        if (p->scope == 0)
          global = p;
        if (p->scope == current_scope)
          return p;
      }
      p = p->link;
    } while (p);
  }
  else
  {
    do
    {
      if (p->scope == 0)
        global = p;
      if (p->scope == current_scope)
        return p;
      p = p->link;
    } while (p);
  }
  return global; // return global label only if no locals were found
}

// Make new empty label with the given name and add it to the label list in
// sorted order. If a label with this name already exists, the new label will
// link to the old label and take its place in the label list.
// No checks are perfomed on the label name, any validity checks should be done
// prior to calling this fuction.
label *newlabel(const std::string &label_name)
{
  label *p = new label;
  p->name = label_name;
  p->scope = 0;
  // find points to the first element in label list that is not less than p.
  // i.e. the first element where `(*find)->name >= p->name`
  auto find = std::lower_bound(labellist.begin(), labellist.end(), p,
                               [](label *const &a, label *const &b) {
                                 return a->name < b->name;
                               });

  if (find != labellist.end() && (*find)->name == p->name)
  { // label already exists with this name
    auto findindex = std::distance(labellist.begin(), find);
    p->link = *find;          // link new label to the old one
    labellist[findindex] = p; // replace the old label
  }
  else
  { // otherwise insert the new label
    p->link = nullptr;
    labellist.insert(find, p);
  }
  return p;
}

void showerror(const std::string &errsrc, const unsigned errline)
{
  error = true;
  fprintf(stderr, "%s(%i): %s\n", errsrc.c_str(), errline, errmsg);

  if (!listerr) // only list the first error for this line
    listerr = errmsg;
}

// process the open file f
void processfile(FILE *f, const std::string &name)
{
  static unsigned nest = 0; // nested include depth
  nest++;

  char fileline[LINEMAX];
  int nline = 0;
  bool eof;
  do
  {
    nline++;
    eof = (fgets(fileline, LINEMAX, f) == NULL);
    if (!eof)
      processline(fileline, name, nline);
  } while (!eof);
  nline--;
  nest--;
  if (nest == 0)
  { // if main source file
    errmsg = nullptr;
    if (iflevel != 0)
      errmsg = NoENDIF;
    if (reptcount != 0)
      errmsg = NoENDR;
    if (makemacro)
      errmsg = NoENDM;
    if (nooutput)
      errmsg = NoENDE;
    if (nonl)
      errmsg = NoENDINL;
    if (errmsg)
      showerror(name, nline);
  }
}

// process single line
// src=source line
// errsrc=source file name
// errline=source file line number
void processline(const std::string &src, const std::string &errsrc, const unsigned errline)
{
  errmsg = nullptr;

  auto src_iter = src.cbegin();
  const auto src_end = src.cend();

  std::string line;
  expandline(line, src_iter, src_end);
  auto line_iter = line.cbegin();
  const auto line_end = line.cend();
  const std::string comment{src_iter, src_end};

  if (macrolevel == 0 || verboselisting)
    listline(line, comment);

  if (errmsg)
  {
    showerror(errsrc.c_str(), errline);
    return;
  }

  if (makemacro)
  { // we're inside a macro definition
    label *p = getreserved(line_iter, line_end);
    errmsg = nullptr;
    auto endmac = line_end;
    if (p == nullptr)
    { // skip over label if there is one, we're looking for "ENDM"
      endmac = line_iter;
      p = getreserved(line_iter, line_end);
    }

    if (p != nullptr && p->value == (ptrdiff_t)endm)
    {
      src_iter = src_end;
      if (endmac != line_end)
      { // TODO - fix
        //endmac[0] = '\n';
        //endmac[1] = '\0'; // hide "ENDM" in case of "label: ENDM"
      }
      else
      { // don't bother adding the last line if its only ENDM
        makemacro = nullptr;
      }
    }

    if (makemacro != nullptr && makemacro != true_ptr)
    { // TODO - fix this jank ass macro pointers
      if (src_iter != src_end)
        line.append(src_iter, src_end); // keep comment for listing
      *makemacro = my_malloc(strlen(line.c_str()) + sizeof(char *) + 1);
      makemacro = (char **)*makemacro;
      *makemacro = nullptr;
      strcpy((char *)&makemacro[1], line.c_str());
    }

    if (p != nullptr && p->value == (ptrdiff_t)endm)
      makemacro = nullptr;
  }
  else if (reptcount != 0)
  { // REPT definition in progress
    label *p = getreserved(line_iter, line_end);
    errmsg = nullptr;
    auto endmac = line_end;
    if (p == nullptr)
    {
      endmac = line_iter;
      p = getreserved(line_iter, line_end);
    }
    if (p != nullptr)
    {
      if (p->value == (ptrdiff_t)rept)
      {
        reptcount++; // keep track of how many ENDR's are needed to finish
      }
      else if (p->value == (ptrdiff_t)endr)
      {
        if ((--reptcount) == 0)
        {
          src_iter = src_end;
          if (endmac != line_end)
          { // TODO - fix
            //endmac[0] = '\n'; // hide "ENDR" in case of "label: ENDR"
            //endmac[1] = 0;
          }
        }
      }
    }
    if (reptcount != 0 || endmac != line_end)
    { // add this line to REPT body
      if (src_iter != src_end)
        line.append(src_iter, src_end); // keep comment for listing
      *makerept = my_malloc(strlen(line.c_str()) + sizeof(char *) + 1);
      makerept = (char **)*makerept;
      *makerept = 0;
      strcpy((char *)&makerept[1], line.c_str());
    }
    if (reptcount == 0)
    { // end of REPT, expand the whole thing right now
      expandrept(errline, errsrc.c_str());
    }
  }
  else
  { // not in a macro or rept definition
    labelhere = nullptr;
    auto line_iter_temp = line_iter;
    label *p = getreserved(line_iter, line_end);
    errmsg = nullptr; // ignore errors?

    if (skipline[iflevel])
    { // conditional assembly.. no code generation
      if (p == nullptr)
      { // p was not a reserved word, ignore it and move on
        p = getreserved(line_iter, line_end);
        if (p == nullptr)
          return;
      }
      if (p->value != (ptrdiff_t)else_ &&
          p->value != (ptrdiff_t)elseif &&
          p->value != (ptrdiff_t)endif &&
          p->value != (ptrdiff_t)if_ &&
          p->value != (ptrdiff_t)ifdef &&
          p->value != (ptrdiff_t)ifndef)
        return; // ignore all other instructions
    }

    if (p == nullptr)
    { // not a reserved word, maybe a label?
      std::string word;
      if (good_label(word, line_iter_temp, line_end))
        addlabel(word, macrolevel > 0);
      if (errmsg)
      {
        showerror(errsrc, errline);
        return;
      }
      p = getreserved(line_iter, line_end);
    }

    if (p != nullptr)
    {
      if (p->type == MACRO) // calling a macro
        expandmacro(p, line_iter, line_end, errline, errsrc);
      else
        ((directive_func)p->value)(p, line_iter, line_end);
    }

    if (!errmsg)
    { // check extra garbage
      line_iter = nonstd::eat_characters(line_iter, line_end, whitespace);
      if (line_iter != line_end)
        errmsg = "Extra characters on line.";
    }
    if (errmsg)
    {
      showerror(errsrc, errline);
    }
  }
}

void showhelp(void)
{
  puts("");
  puts("asm6f " VERSION " (+ freem modifications)\n");
  puts("Usage:  asm6f [-options] sourcefile [outputfile] [listfile]\n");
  puts("\t-?\t\tshow this help");
  puts("\t-l\t\tcreate listing");
  puts("\t-L\t\tcreate verbose listing (expand REPT, MACRO)");
  puts("\t-d<name>\tdefine symbol");
  puts("\t-q\t\tquiet mode (no output unless error)");
  // [additions from various sources (freem, nicklausw, Sour)]
  puts("\t-n\t\texport FCEUX-compatible .nl files");
  puts("\t-f\t\texport Lua symbol file");
  puts("\t-c\t\texport .cdl for use with FCEUX/Mesen");
  puts("\t-m\t\texport Mesen-compatible label file (.mlb)\n");
  puts("See README.TXT for more info.\n");
}

int main(int argc, char **argv)
{
  char str[512];
  char *nameptr;
  FILE *f;

  if (argc < 2)
  {
    showhelp();
    return EXIT_FAILURE;
  }
  initlabels();
  comments = std::vector<comment *>();

  // Parse command line arguments
  unsigned notoption = 0;
  for (int i = 1; i < argc; i++)
  {
    if (*argv[i] == '-')
    {
      switch (argv[i][1])
      {
      case '-': // long options
        if (i + 1 >= argc)
          fatal_error("missing option for %s", argv[i]);
        if (strcmp(argv[i], "--path") == 0)
        {
          i++;
          includepath = argv[i];
        }
        else
        {
          fatal_error("unknown option: %s", argv[i]);
        }
        break;
      case 'h': // display help
      case '?':
        showhelp(); // should this be a failure?
        return EXIT_FAILURE;
      case 'L': // verbose listing
        verboselisting = true;
      case 'l': // generate list file
        listfilename = static_cast<char *>(true_ptr);
        break;
      case 'd':
        if (argv[i][2])
        {
          if (findlabel(my_strdup(&argv[i][2])) == nullptr)
          {
            label *p = newlabel(my_strdup(&argv[i][2]));
            p->type = VALUE;
            p->value = 1;
            p->kitchen_sink = static_cast<char *>(true_ptr);
            p->pass = 0;
          }
        }
        break;
      case 'q': // quiet
        verbose = false;
        break;
      case 'n': // generate .nl file
        genfceuxnl = true;
        break;
      case 'm': // generate messen label file
        genmesenlabels = true;
        break;
      case 'c': // generate .cdl file
        gencdl = true;
        break;
      case 'f': // generate lua file
        genlua = true;
        break;
      default:
        fatal_error("unknown option: %s", argv[i]);
      }
    }
    else
    {
      if (notoption == 0)
        inputfilename = argv[i];
      else if (notoption == 1)
        outputfilename = argv[i];
      else if (notoption == 2)
        listfilename = argv[i];
      else
        fatal_error("unused argument: %s", argv[i]);
      notoption++;
    }
  }

  if (!inputfilename)
    fatal_error("No source file specified.");

  strcpy(str, inputfilename);
  nameptr = strrchr(str, '.'); // nameptr='.' ptr
  if (nameptr)
    if (strchr(nameptr, '\\'))
      nameptr = 0; // watch out for "dirname.ext\listfile"
  if (!nameptr)
    nameptr = str + strlen(str); // nameptr=inputfile extension
  if (!outputfilename)
  {
    strcpy(nameptr, ".bin");
    outputfilename = my_strdup(str);
  }

  if (listfilename == true_ptr)
  { // if listfile was wanted but no name was specified, use srcfile.lst
    strcpy(nameptr, ".lst");
    listfilename = my_strdup(str);
  }

  f = fopen(inputfilename, "rb"); // if srcfile won't open, try some default extensions
  if (!f)                         // TODO should we really do this?
  {
    strcpy(nameptr, ".asm");
    f = fopen(str, "rb");
    if (!f)
    {
      strcpy(nameptr, ".s");
      f = fopen(str, "rb");
    }
    if (f)
      inputfilename = my_strdup(str);
  }
  if (f)
    fclose(f);

  if (gencdl)
  {
    strcpy(nameptr, ".cdl");
    cdlfilename = my_malloc(strlen(nameptr) + 1);
    strcpy(cdlfilename, str);
  }

  // main assembly loop:
  label *p = nullptr;
  do
  {
    filepos = 0;
    current_pass++;
    if (current_pass == MAXPASSES || (p == lastlabel))
      lastchance = true; // give up on too many tries or no progress made
    if (lastchance)
      message("last try..\n");
    else
      message("pass %i..\n", current_pass);
    needanotherpass = false;
    skipline[0] = false;
    current_scope = 1;
    nextscope = 2;
    defaultfiller = DEFAULTFILLER; // reset filler value
    addr = NOORIGIN;               // undefine origin
    p = lastlabel;

    const std::string filename_str{inputfilename};
    auto filename_iter = filename_str.cbegin();
    include(nullptr, filename_iter, filename_str.cend()); // start assembling srcfile

    if (errmsg)
    { // todo - shouldn't this set error?
      fputs(errmsg, stderr);
    }
  } while (!error && !lastchance && needanotherpass); // while no hard errors, not final try, and labels are still unresolved

  if (outputfile)
  {
    // Be sure last of output file is written properly
    int result;
    if (fwrite(outputbuff, 1, outcount, outputfile) < (size_t)outcount || fflush(outputfile))
      fatal_error("Write error.");

    long i = ftell(outputfile);

    result = fclose(outputfile);
    outputfile = nullptr; // prevent fatal_error() from trying to close file again
    if (result)
      fatal_error("Write error.");

    if (!error)
    {
      message("%s written (%i bytes).\n", outputfilename, i);
    }
    else
      remove(outputfilename);
  }
  else
  {
    if (!error)
      fputs("nothing to do!", stderr);
    error = true;
  }
  if (listfile != nullptr)
    listline(nullptr, nullptr);

  // [freem addition] only generate labelfiles if asked
  if (genfceuxnl)
    export_labelfiles();
  if (genlua)
    export_lua();
  if (genmesenlabels)
    export_mesenlabels();

  return error ? EXIT_FAILURE : 0;
}

byte listbuff[LISTMAX];
int listcount;
void output(const byte *p, int size, int cdlflag)
{
  static unsigned oldpass = 0;
  /*  static int noentry=0;
  if(addr<0) {
    if(!noentry) {// do this only once
      noentry++;
      if(lastchance) errmsg=NoOrigin;// "Origin undefined."
    }
    return;
  }*/
  if (gencdl)
  {
    if (oldpass != current_pass)
    {
      if (cdlfile)
      {
        fclose(cdlfile);
      }
      cdlfile = fopen(cdlfilename, "wb");
    }

    if (cdlfile && filepos >= 16)
    {
      int repeat = size;
      while (repeat--)
      {
        if (addr < 0x10000)
        {
          // PRG, mark as either code or data
          byte flag = (byte)cdlflag;
          fwrite((void *)&flag, 1, 1, cdlfile);
        }
        else
        {
          // CHR data
          fwrite("\x0", 1, 1, cdlfile);
        }
      }
    }
  }

  addr += size;

  if (nooutput)
    return;
  if (oldpass != current_pass)
  {
    oldpass = current_pass;
    if (outputfile)
      fclose(outputfile);
    outputfile = fopen(outputfilename, "wb");
    filepos = 0;
    outcount = 0;
    if (!outputfile)
    {
      errmsg = "Can't create output file.";
      return;
    }

    // insert iNES header if needed
    if (ines_include)
    {
      byte ines_header[16] = {'N', 'E', 'S', 0x1A,
                              static_cast<byte>(inesprg_num),
                              static_cast<byte>(ineschr_num),
                              static_cast<byte>((inesmap_num << 4) | inesmir_num),
                              static_cast<byte>((inesmap_num & 0xF0) | (use_nes2 << 3) | (nes2tv_num << 7)),
                              static_cast<byte>((inesmap_num >> 8) | (nes2sub_num << 4)),
                              static_cast<byte>((inesprg_num >> 8) | ((ineschr_num >> 8) << 4)),
                              static_cast<byte>((nes2bram_num << 4) | nes2prg_num),
                              static_cast<byte>((nes2chrbram_num << 4) | nes2chr_num),
                              static_cast<byte>(nes2tv_num),
                              0x00, 0x00, 0x00};
      if (fwrite(ines_header, 1, 16, outputfile) < (size_t)16 || fflush(outputfile))
        errmsg = "Write error.";
      filepos += 16;
    }
  }
  if (!outputfile)
    return;
  while (size--)
  {
    if (listfile && listcount < LISTMAX)
      listbuff[listcount] = *p;
    listcount++;
    filepos++;
    outputbuff[outcount++] = *p;
    p++;
    if (outcount >= BUFFSIZE)
    {
      if (fwrite(outputbuff, 1, BUFFSIZE, outputfile) < BUFFSIZE)
        errmsg = "Write error.";
      outcount = 0;
    }
  }
}

// Outputs integer as little-endian.
static void output_le(int n, int size, int cdlflag)
{
  byte b[2];
  b[0] = n;
  b[1] = n >> 8;
  output(b, size, cdlflag);
}

// end listing when src is empty string
void listline(const std::string &src, const std::string &comment)
{
  static unsigned oldpass = 0;
  std::string srcbuff;

  if (listfilename == nullptr)
    return;

  if (oldpass != current_pass)
  { // new pass => new listfile
    oldpass = current_pass;
    if (listfile)
      fclose(listfile);
    listfile = fopen(listfilename, "w");
    if (listfile == nullptr)
    {
      listfilename = nullptr; // stop trying
      // todo - if user wants a listing, this SHOULD be an error, otherwise
      // he might still have old listing and think it's the current one.
      // For example, he might have had it open in a text editor, preventing its
      // creation here.
      fputs("Can't create list file.", stderr); // not critical, just give a warning
      return;
    }
  }
  else
  { // finish previous line
    int i = 0;
    for (; i < listcount && i < LISTMAX; i++)
      fprintf(listfile, " %02X", (int)listbuff[i]);
    for (; i < LISTMAX; i++)
      fprintf(listfile, "   ");
    fputs(listcount > LISTMAX ? ".. " : "   ", listfile);
    fputs(srcbuff.c_str(), listfile);
    if (listerr)
    {
      fprintf(listfile, "*** %s\n", listerr);
      listerr = 0;
    }
  }

  listcount = 0;

  if (src.empty())
  { // TODO - move this out of function
    fclose(listfile);
    message("%s written.\n", listfilename);
    return;
  }

  if (addr < 0)
    fprintf(listfile, "\t ");
  else
    fprintf(listfile, "%05X", (int)addr);
  srcbuff = src; // make a copy of the original source line
  if (!comment.empty())
  {
    srcbuff += comment;
    if (genmesenlabels && filepos > 0 && addr < 0x10000)
    { // save this comment - needed for export
      addcomment(comment.c_str());
    }
  }
}

// Directives:
// ------------------------------------------------------
// <directive>(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
//
//  id: reserved word
//  **next: source line (ptr should be moved past directive on exit)
// ------------------------------------------------------
void equ(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  if (!labelhere)
    errmsg = NeedName; // EQU without a name
  else
  {
    if (labelhere->type == LABEL)
    { // new EQU
      // eat whitespace off both ends
      next = nonstd::eat_characters(next, end, whitespace);
      const auto stop = nonstd::eat_characters(std::reverse_iterator(end), std::reverse_iterator(next), whitespace).base();
      const std::string s{next, stop};
      next = stop;

      if (!s.empty())
      {
        labelhere->type = EQUATE;
        labelhere->kitchen_sink = my_strdup(s.c_str());
      }
      else
      {
        errmsg = IncompleteExp;
      }
    }
    else if (labelhere->type != EQUATE)
    {
      errmsg = LabelDefined;
    }
    //*s = 0; // end line
  }
}

void equal(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  if (labelhere == nullptr)
  {
    errmsg = NeedName; // `=` without a name
    return;
  }

  labelhere->type = VALUE;
  dependant = 0;
  labelhere->value = eval(next, end, WHOLEEXP);
  labelhere->kitchen_sink = reinterpret_cast<char *>(!dependant ? true_ptr : nullptr);
}

void base(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  int val;
  dependant = 0;
  val = eval(next, end, WHOLEEXP);
  if (!dependant && !errmsg)
    addr = val;
  else
    addr = NOORIGIN; // undefine origin
}

// nothing to do (empty line)
void nothing(label *id, std::string::const_iterator &next, const std::string::const_iterator &end) noexcept
{
}

void include(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  const std::string filename{next, end};
  FILE *f = getfile(filename.c_str(), "r+"); // read as text, the + makes recursion not possible
  // TODO cache files for next pass
  if (f == nullptr)
  {
    errmsg = CantOpen;
    error = true;
  }
  else
  {
    processfile(f, filename);
    fclose(f);
    errmsg = nullptr; // let main know file was ok
  }
  next = end; // need to play safe because this could be the main srcfile
}

void incbin(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  FILE *f = getfile(std::string(next, end), "rb");
  if (f == nullptr)
  {
    errmsg = CantOpen;
    return;
  }

  fseek(f, 0, SEEK_END);
  int filesize = ftell(f);

  int seekpos = 0;
  if (next_arg(next, end))
    seekpos = eval(next, end, WHOLEEXP);
  if (!errmsg && !dependant)
    if (seekpos < 0 || seekpos > filesize)
      errmsg = SeekOutOfRange;
  if (errmsg)
  {
    if (f != nullptr)
      fclose(f);
    return;
  }
  fseek(f, seekpos, SEEK_SET);
  // get size:
  int bytesleft;
  if (next_arg(next, end))
  {
    bytesleft = eval(next, end, WHOLEEXP);
    if (!errmsg && !dependant)
      if (bytesleft < 0 || bytesleft > (filesize - seekpos))
        errmsg = BadIncbinSize;
    if (errmsg)
    {
      if (f != nullptr)
        fclose(f);
      return;
    }
  }
  else
  {
    bytesleft = filesize - seekpos;
  }
  // read file:
  int i;
  while (bytesleft)
  {
    if (bytesleft > BUFFSIZE)
      i = BUFFSIZE;
    else
      i = bytesleft;
    fread(inputbuff, 1, i, f);
    output(inputbuff, i, DATA);
    bytesleft -= i;
  }

  if (f != nullptr)
    fclose(f);

  next = end;
}

void hex(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  next = nonstd::eat_characters(next, end, whitespace);
  if (next == end)
  {
    errmsg = MissingOperand;
    return;
  }

  std::basic_string<byte> bytes;
  while (next != end)
  {
    byte b = from_hex(*next);
    next++;
    if (next != end && std::isxdigit(*next))
    {
      b = (b << 4) + from_hex(*next);
      next++;
    }
    bytes += b;
    next = nonstd::eat_characters(next, end, whitespace);
  }
  output(bytes.c_str(), bytes.size(), DATA);
}

void dw(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  int val;
  do
  {
    val = eval(next, end, WHOLEEXP);
    if (!errmsg)
    {
      if (val > 65535 || val < -65536)
        errmsg = OutOfRange;
      else
        output_le(val, 2, DATA);
    }
  } while (!errmsg && next_arg(next, end));
}

void dl(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  byte val;
  do
  {
    val = eval(next, end, WHOLEEXP) & 0xff;
    if (!errmsg)
      output(&val, 1, DATA);
  } while (!errmsg && next_arg(next, end));
}

void dh(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  byte val;
  do
  {
    val = eval(next, end, WHOLEEXP) >> 8;
    if (!errmsg)
      output(&val, 1, DATA);
  } while (!errmsg && next_arg(next, end));
}

void db(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  do
  {
    next = nonstd::eat_characters(next, end, whitespace);
    const char quot = *next;
    if (quot == '"' || quot == '\'')
    { // string TODO allow db's with strings and commas
      auto str_begin = next + 1;
      while (next != end && *(next - 1) == '\\')
        next = std::find(++next, end, quot);
      if (next == end)
      { // no terminating quot
        errmsg = IncompleteExp;
        continue;
      }
      // next now points to terminating quot character
      // check for string shift
      std::string temp{next, end};
      temp[0] = '0'; // evaluate the shift value
      auto it = temp.cbegin();
      int shift = eval(it, temp.cend(), WHOLEEXP);
      if (errmsg)
        break;
      while (str_begin != next)
      {
        if (*str_begin == '\\')
          str_begin++; // garunteed to not run off end from checks above
        output_le(*str_begin + shift, 1, DATA);
        str_begin++;
      }
    }
    else
    {
      int val = eval(next, end, WHOLEEXP);
      if (!errmsg)
      {
        if (val > 255 || val < -128)
          errmsg = OutOfRange;
        else
          output_le(val, 1, DATA);
      }
    }
  } while (!errmsg && next_arg(next, end));
}

void dsw(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  int count;
  int val = defaultfiller;
  dependant = 0;
  count = eval(next, end, WHOLEEXP);
  if (dependant || (count < 0 && needanotherpass)) // unknown count! don't do anything
    count = 0;
  if (next_arg(next, end))
    val = eval(next, end, WHOLEEXP);
  if (!errmsg && !dependant)
    if (val > 65535 || val < -32768 || count < 0)
      errmsg = OutOfRange;
  if (errmsg)
    return;
  while (count--)
    output_le(val, 2, DATA);
}

void filler(int count, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  int val = defaultfiller;
  if (dependant || (count < 0 && needanotherpass)) // unknown count! don't do anything
    count = 0;
  if (next_arg(next, end))
    val = eval(next, end, WHOLEEXP);
  if (!errmsg && !dependant)
    if (val > 255 || val < -128 || count < 0 || count > 0x100000)
      errmsg = OutOfRange;
  if (errmsg)
    return;
  while (count--) // !#@$
    output_le(val, 1, NONE);
}

void dsb(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  int count;
  dependant = 0;
  count = eval(next, end, WHOLEEXP);
  filler(count, next, end);
}

void align(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  dependant = 0;
  unsigned count = eval(next, end, WHOLEEXP);
  if (count >= 0)
  {
    if ((unsigned int)addr % count)
      count -= (unsigned int)addr % count;
    else
      count = 0;
  }
  else
    count = 0;
  filler(count, next, end);
}

void pad(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  int count;
  if (addr < 0)
  {
    errmsg = UndefinedPC;
  }
  else
  {
    dependant = 0;
    count = eval(next, end, WHOLEEXP) - addr;
    filler(count, next, end);
  }
}

void org(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  if (addr < 0)
    base(id, next, end); // this is the first ORG; PC hasn't been set yet
  else
    pad(id, next, end);
}

// Tries to take substr from begin.
// whitespace and case are ignored.
// returns true if the entire substr was found.
bool take_string(std::string::const_iterator &begin, const std::string::const_iterator &end, const std::string substr)
{
  if (substr.empty())
    return true;

  for (const char &c : substr)
  {
    begin = nonstd::eat_characters(begin, end, whitespace);
    if (begin == end || std::tolower(*begin) != std::tolower(c))
      return false;
    begin++;
  }
  return true;
}

void opcode(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  const bool anotherpass_temp = needanotherpass;
  int forceRel = 0;

  if (!allowunstable)
    for (int uns = 0; uns < 4; uns++)
      ;   // TODO //if (!strcmp(id->name, unstablelist[uns]))
          //  fatal_error("Unstable instruction \"%s\" used without calling UNSTABLE.", id->name);

  const std::map<optype, byte> modes = *(reinterpret_cast<std::map<optype, byte> *>(id->kitchen_sink)); // TODO this is disgusting
  int val = 0;
  for (const auto &mode : modes)
  { // loop through all allowed addressing modes for this instruction
    auto next_iter = next;
    needanotherpass = anotherpass_temp;
    dependant = false;
    errmsg = nullptr;
    const optype type = mode.first;

    if (!take_string(next_iter, end, ophead[type]))
      continue; // operand head dosent match

    if (opsize[type] != 0)
    { // evaluate operand
      val = eval(next_iter, end, WHOLEEXP);
      if (type == REL)
      {
        if (!dependant)
        {
          val -= addr + 2;
          if (val > 127 || val < -128)
          {
            needanotherpass = true; // give labels time to sort themselves out
            if (lastchance)
            { // labels are as resolved as theyre going to be and branch is still out of range
              errmsg = "Branch out of range.";
              forceRel = 1;
            }
          }
        }
      }
      else if (opsize[type] == 1)
      {
        if (!dependant)
        {
          if (val > 255 || val < -128)
            errmsg = OutOfRange;
        }
        else if (type != IMM)
        { // default to non-ZP instruction
          continue;
        }
      }
      else // opsize[type] > 1
      {
        if ((val < 0 || val > 0xffff) && !dependant)
          errmsg = OutOfRange;
      }

      if (errmsg && !dependant && !forceRel)
        continue;
    }

    // opcode tail should match head
    if (!take_string(next_iter, end, optail[type]))
      continue; // tail missing
    next_iter = nonstd::eat_characters(next_iter, end, whitespace);
    if (next_iter != end)
      continue; // extra tail

    if (addr > 0xffff)
      errmsg = "PC out of range.";
    output(&mode.second, sizeof(mode.second), CODE);
    output_le(val, opsize[type], CODE);
    next = next_iter;
    return;
  }
  if (!errmsg)
    errmsg = Illegal;
}

void if_(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  int val;
  if (iflevel >= IFNESTS - 1)
    errmsg = IfNestLimit;
  else
    iflevel++;
  dependant = 0;
  val = eval(next, end, WHOLEEXP);
  if (dependant || errmsg)
  { // don't process yet
    ifdone[iflevel] = true;
    skipline[iflevel] = true;
  }
  else
  {
    skipline[iflevel] = !val || skipline[iflevel - 1];
    ifdone[iflevel] = !skipline[iflevel];
  }
}

void ifdef(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  if (iflevel >= IFNESTS - 1)
    errmsg = IfNestLimit;
  else
    iflevel++;
  std::string temp;
  good_label(temp, next, end);
  skipline[iflevel] = (findlabel(temp) == nullptr) || skipline[iflevel - 1];
  ifdone[iflevel] = !skipline[iflevel];
}

void ifndef(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  if (iflevel >= IFNESTS - 1)
    errmsg = IfNestLimit;
  else
    iflevel++;
  std::string temp;
  good_label(temp, next, end);
  skipline[iflevel] = (findlabel(temp) != nullptr) || skipline[iflevel - 1];
  ifdone[iflevel] = !skipline[iflevel];
}

void elseif(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  int val;
  if (iflevel != 0)
  {
    dependant = 0;
    val = eval(next, end, WHOLEEXP);
    if (!ifdone[iflevel])
    { // no previous true statements
      if (dependant || errmsg)
      { // don't process yet
        ifdone[iflevel] = true;
        skipline[iflevel] = true;
      }
      else
      {
        skipline[iflevel] = !val || skipline[iflevel - 1];
        ifdone[iflevel] = !skipline[iflevel];
      }
    }
    else
    {
      skipline[iflevel] = 1;
    }
  }
  else
  {
    errmsg = "ELSEIF without IF.";
  }
}

void else_(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  if (iflevel != 0)
    skipline[iflevel] = ifdone[iflevel] || skipline[iflevel - 1];
  else
    errmsg = "ELSE without IF.";
}

void endif(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  if (iflevel != 0)
    --iflevel;
  else
    errmsg = "ENDIF without IF.";
}

void endm(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{ // ENDM is handled during macro definition (see processline)
  errmsg = ExtraENDM;
}

void endr(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{ // ENDR is handled during macro definition (see processline)
  errmsg = ExtraENDR;
}

// create macro
void macro(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  std::string word;
  unsigned param_count;
  labelhere = nullptr;

  if (good_label(word, next, end))
    addlabel(word, false);
  else
    errmsg = NeedName;

  makemacro = static_cast<char **>(true_ptr); // flag for processline to skip to ENDM
  if (errmsg)
  { // no valid macro name
    return;
  }

  // jank check if this is a new label?
  if (labelhere->type == LABEL)
  { // new macro
    labelhere->type = MACRO;
    labelhere->kitchen_sink = 0;
    makemacro = &labelhere->kitchen_sink;
    // build param list
    param_count = 0;
    auto src = next;
    while (good_label(word, src, end))
    {
      next = src; // this is some wack ass pointer magic
      *makemacro = my_malloc(strlen(word.c_str()) + sizeof(char *) + 1);
      makemacro = reinterpret_cast<char **>(*makemacro);
      strcpy(reinterpret_cast<char *>(&makemacro[1]), word.c_str());
      param_count++;
      next_arg(src, end);
    }
    errmsg = nullptr;               // remove good_label's errmsg
    labelhere->value = param_count; // set param count
    *makemacro = nullptr;
  }
  else if (labelhere->type != MACRO)
  {
    errmsg = LabelDefined;
  }
  else
  { // macro was defined on a previous pass.. skip past params
    next = end;
  }
}

// call to a macro
// errline=source file line number
// errsrc=source file name
void expandmacro(label *id, std::string::const_iterator &next, const std::string::const_iterator &end, unsigned errline, const std::string &errsrc)
{
  if (id->used)
  {
    errmsg = RecurseMACRO;
    return;
  }
  id->used = true;

  int linecount = 0;
  const unsigned oldscope = current_scope; // watch those nested macros..
  current_scope = nextscope++;
  macrolevel++;
  char **line = (char **)(id->kitchen_sink);

  std::string macroerr;
  macroerr.reserve(WORDMAX * 2);
  std::snprintf(macroerr.data(), macroerr.capacity(), "%s(%i):%s", errsrc.c_str(), errline, id->name.c_str());

  // read in arguments
  auto arg_iter = nonstd::eat_characters(next, end, whitespace);
  unsigned arg_count = id->value; // parameter count
  unsigned arg = 0;
  while (arg_iter != end)
  {
    const std::string temp{",'\""};
    auto arg_end = std::find_first_of(arg_iter, end, temp.cbegin(), temp.cend());
    const char c = arg_end != end ? *arg_end : '\0';
    if (c == '"' || c == '\'')
    {
      do // go to end of string
        arg_end = std::find(++arg_end, end, c);
      while (arg_end != end && *(arg_end - 1) == '\\');
      if (arg_end == end)
      {
        errmsg = "Unterminated string.";
        return;
      }
    }

    if (arg < arg_count)
    {                                       // make named arg, god this is jank
      addlabel({(char *)&(line[1])}, true); // set labelhere to new label
      equ(nullptr, arg_iter, arg_end);      //
      line = (char **)*line;                // next arg name
    }
    arg++;
    arg_iter = arg_end;
    next_arg(arg_iter, end);
  }
  next = arg_iter;

  // make "\?" arg
  // {..}  what

  while (arg++ < arg_count) // skip any unused arg names
    line = (char **)*line;

  while (line != nullptr)
  {
    linecount++;
    processline((char *)&(line[1]), macroerr, linecount);
    line = (char **)*line;
  }
  errmsg = nullptr;
  current_scope = oldscope;
  macrolevel--;
  id->used = false;
}

int rept_loops;
char *repttext; // rept chain begins here
void rept(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  dependant = false;
  rept_loops = eval(next, end, WHOLEEXP);
  if (dependant || errmsg || rept_loops < 0)
    rept_loops = 0;
  makerept = &repttext;
  repttext = 0;
  reptcount++; // tell processline to start storing up rept lines
}

void expandrept(int errline, const char *const errsrc)
{
  char macroerr[WORDMAX * 2]; // source to show in listing (this should be enough, i hope?)
  char **start, **line;
  int linecount;
  int oldscope;

  start = (char **)repttext; // first rept data
  oldscope = current_scope;
  macrolevel++;
  for (int i = rept_loops; i; --i)
  {
    linecount = 0;
    current_scope = nextscope++;
    sprintf(macroerr, "%s(%i):REPT", errsrc, errline);
    line = start;
    while (line)
    {
      linecount++;
      processline((char *)&line[1], macroerr, linecount);
      line = (char **)*line;
    }
  }
  while (start)
  { // delete everything
    line = (char **)*start;
    free(start);
    start = line;
  }
  errmsg = 0;
  current_scope = oldscope;
  macrolevel--;
}

int enum_saveaddr;
void enum_(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  dependant = 0;
  int val = eval(next, end, WHOLEEXP);
  if (!nooutput)
    enum_saveaddr = addr;
  addr = val;
  nooutput = true;
}

void ende(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  if (nooutput)
  {
    addr = enum_saveaddr;
    nooutput = false;
  }
  else
  {
    errmsg = ExtraENDE;
  }
}

void ignorenl(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  nonl = true;
}

void endinl(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  if (nonl)
    nonl = false;
  else
    errmsg = ExtraENDINL;
}

void fillval(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  dependant = false; // why?
  defaultfiller = eval(next, end, WHOLEEXP);
}

void make_error(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  const std::string message{next, end};
  errmsg = message.c_str();
  error = true;
  next = end;
}

void unstable(label *id, std::string::const_iterator &next, const std::string::const_iterator &end) noexcept
{
  allowunstable = true;
}

// iNES header functions

void inesprg(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  inesprg_num = eval(next, end, WHOLEEXP);

  if (inesprg_num < 0 || inesprg_num > 0xFF)
    errmsg = OutOfRange;

  ines_include = true;
}

void ineschr(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  ineschr_num = eval(next, end, WHOLEEXP);

  if (ineschr_num < 0 || ineschr_num > 0xFF)
    errmsg = OutOfRange;

  ines_include = true;
}

void inesmir(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  inesmir_num = eval(next, end, WHOLEEXP);

  // force 4 bits
  if (inesmir_num > 16 || inesmir_num < 0)
    errmsg = OutOfRange;

  ines_include = true;
}

void inesmap(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  inesmap_num = eval(next, end, WHOLEEXP);

  // ines 2.0 allows for some big numbers...
  if (inesmap_num > 4095 || inesmap_num < 0)
    errmsg = OutOfRange;

  ines_include = true;
}

void nes2chrram(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  nes2chr_num = eval(next, end, WHOLEEXP);

  if (nes2chr_num < 0 || nes2chr_num > 16)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}

void nes2prgram(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  nes2prg_num = eval(next, end, WHOLEEXP);

  if (nes2prg_num < 0 || nes2prg_num > 16)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}

void nes2sub(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  nes2sub_num = eval(next, end, WHOLEEXP);

  if (nes2sub_num < 0 || nes2sub_num > 16)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}

void nes2tv(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  nes2tv_num = eval(next, end, WHOLEEXP);

  // possible presets...
  if (nes2tv_num == 'N')
    nes2tv_num = 0;
  if (nes2tv_num == 'P')
    nes2tv_num = 1;

  // might just change to 'N', 'P' but eh...
  if (nes2tv_num == 'B')
    nes2tv_num = 2;

  if (nes2tv_num > 2 || nes2tv_num < 0)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}

void nes2vs(label *id, std::string::const_iterator &next, const std::string::const_iterator &end) noexcept
{
  nes2vs_num = 1;
  ines_include = true;
  use_nes2 = 1;
}

void nes2bram(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  nes2bram_num = eval(next, end, WHOLEEXP);

  if (nes2bram_num < 0 || nes2bram_num > 16)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}

void nes2chrbram(label *id, std::string::const_iterator &next, const std::string::const_iterator &end)
{
  nes2chrbram_num = eval(next, end, WHOLEEXP);

  if (nes2chrbram_num < 0 || nes2chrbram_num > 16)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}