
#include "asm6f++.hpp"

#include <vector>
#include <iostream>
#include <algorithm>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include <stdarg.h>

// what even the fuck
static void *true_ptr = &true_ptr;

unsigned pass = 0;  // current assembly pass
unsigned scope;     // current scope, 0=global
unsigned nextscope; // next nonglobal scope (increment on each new block of localized code)

bool lastchance = false; // set on final attempt
bool needanotherpass;    // still need to take care of some things..
bool error = false;      // hard error (stop assembly after this pass)
const char *errmsg;

char **makemacro = 0;  // (during macro creation) where next macro line will go.  1 to skip past macro
char **makerept;       // like makemacro.. points to end of string chain
int macrolevel = 0;    // number of nested macro/rept being expanded
int reptcount = 0;     // counts rept statements during rept string storage
unsigned iflevel = 0;  // index into ifdone[],skipline[]
int ifdone[IFNESTS];   // nonzero if current IF level has been true
int skipline[IFNESTS]; // 1 on an IF statement that is false

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

bool nooutput = false;       // supress output (used with ENUM)
bool nonl = false;           // supress output to .nl files (used with IGNORENL)
bool allowunstable = false;  // allow unstable instrucitons

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
int filepos = 0;    // [freem addition (from asm6_sonder.c)] <- useless comment

std::vector<comment *> comments;
int lastcommentpos = -1;
int commentcount;

std::vector<label *> labellist; // all of the labels allocated thus far
label *lastlabel;               // last label created
label *labelhere;               // points to the label being defined on the current line (for EQU, =, etc)
label firstlabel = {            // '$' label
    "$",               // *name
    0,                 // value
    0,                 // pos
    (char *)&true_ptr, // *line
    VALUE,             // type
    false,             // used
    0,                 // pass
    0,                 // scope
    false,             // ignorenl
    nullptr,           // link
};


const unsigned opsize[] = {0, 1, 2, 1, 1, 1, 1, 2, 2, 1, 2, 1, 0};
const char ophead[] = {0, '#', '(', '(', '(', 0, 0, 0, 0, 0, 0, 0, 0};
const char *optail[] = {"A", "", ")", ",X)", "),Y", ",X", ",Y", ",X", ",Y", "", "", "", ""};

const byte brk[] = {0x00, IMM, 0x00, ZP, 0x00, IMP, END_BYTE};
const byte ora[] = {0x09, IMM, 0x01, INDX, 0x11, INDY, 0x15, ZPX, 0x1d, ABSX, 0x19, ABSY, 0x05, ZP, 0x0d, ABS, END_BYTE};
const byte asl[] = {0x0a, ACC, 0x16, ZPX, 0x1e, ABSX, 0x06, ZP, 0x0e, ABS, 0x0a, IMP, END_BYTE};
const byte php[] = {0x08, IMP, END_BYTE};
const byte bpl[] = {0x10, REL, END_BYTE};
const byte clc[] = {0x18, IMP, END_BYTE};
const byte jsr[] = {0x20, ABS, END_BYTE};
const byte and_[] = {0x29, IMM, 0x21, INDX, 0x31, INDY, 0x35, ZPX, 0x3d, ABSX, 0x39, ABSY, 0x25, ZP, 0x2d, ABS, END_BYTE};
const byte bit[] = {0x24, ZP, 0x2c, ABS, END_BYTE};
const byte rol[] = {0x2a, ACC, 0x36, ZPX, 0x3e, ABSX, 0x26, ZP, 0x2e, ABS, 0x2a, IMP, END_BYTE};
const byte plp[] = {0x28, IMP, END_BYTE};
const byte bmi[] = {0x30, REL, END_BYTE};
const byte sec[] = {0x38, IMP, END_BYTE};
const byte rti[] = {0x40, IMP, END_BYTE};
const byte eor[] = {0x49, IMM, 0x41, INDX, 0x51, INDY, 0x55, ZPX, 0x5d, ABSX, 0x59, ABSY, 0x45, ZP, 0x4d, ABS, END_BYTE};
const byte lsr[] = {0x4a, ACC, 0x56, ZPX, 0x5e, ABSX, 0x46, ZP, 0x4e, ABS, 0x4a, IMP, END_BYTE};
const byte pha[] = {0x48, IMP, END_BYTE};
const byte jmp[] = {0x6c, IND, 0x4c, ABS, END_BYTE};
const byte bvc[] = {0x50, REL, END_BYTE};
const byte cli[] = {0x58, IMP, END_BYTE};
const byte rts[] = {0x60, IMP, END_BYTE};
const byte adc[] = {0x69, IMM, 0x61, INDX, 0x71, INDY, 0x75, ZPX, 0x7d, ABSX, 0x79, ABSY, 0x65, ZP, 0x6d, ABS, END_BYTE};
const byte ror[] = {0x6a, ACC, 0x76, ZPX, 0x7e, ABSX, 0x66, ZP, 0x6e, ABS, 0x6a, IMP, END_BYTE};
const byte pla[] = {0x68, IMP, END_BYTE};
const byte bvs[] = {0x70, REL, END_BYTE};
const byte sei[] = {0x78, IMP, END_BYTE};
const byte sta[] = {0x81, INDX, 0x91, INDY, 0x95, ZPX, 0x9d, ABSX, 0x99, ABSY, 0x85, ZP, 0x8d, ABS, END_BYTE};
const byte sty[] = {0x94, ZPX, 0x84, ZP, 0x8c, ABS, END_BYTE};
const byte stx[] = {0x96, ZPY, 0x86, ZP, 0x8e, ABS, END_BYTE};
const byte dey[] = {0x88, IMP, END_BYTE};
const byte txa[] = {0x8a, IMP, END_BYTE};
const byte bcc[] = {0x90, REL, END_BYTE};
const byte tya[] = {0x98, IMP, END_BYTE};
const byte txs[] = {0x9a, IMP, END_BYTE};
const byte ldy[] = {0xa0, IMM, 0xb4, ZPX, 0xbc, ABSX, 0xa4, ZP, 0xac, ABS, END_BYTE};
const byte lda[] = {0xa9, IMM, 0xa1, INDX, 0xb1, INDY, 0xb5, ZPX, 0xbd, ABSX, 0xb9, ABSY, 0xa5, ZP, 0xad, ABS, END_BYTE};
const byte ldx[] = {0xa2, IMM, 0xb6, ZPY, 0xbe, ABSY, 0xa6, ZP, 0xae, ABS, END_BYTE};
const byte tay[] = {0xa8, IMP, END_BYTE};
const byte tax[] = {0xaa, IMP, END_BYTE};
const byte bcs[] = {0xb0, REL, END_BYTE};
const byte clv[] = {0xb8, IMP, END_BYTE};
const byte tsx[] = {0xba, IMP, END_BYTE};
const byte cpy[] = {0xc0, IMM, 0xc4, ZP, 0xcc, ABS, END_BYTE};
const byte cmp[] = {0xc9, IMM, 0xc1, INDX, 0xd1, INDY, 0xd5, ZPX, 0xdd, ABSX, 0xd9, ABSY, 0xc5, ZP, 0xcd, ABS, END_BYTE};
const byte dec[] = {0xd6, ZPX, 0xde, ABSX, 0xc6, ZP, 0xce, ABS, END_BYTE};
const byte iny[] = {0xc8, IMP, END_BYTE};
const byte dex[] = {0xca, IMP, END_BYTE};
const byte bne[] = {0xd0, REL, END_BYTE};
const byte cld[] = {0xd8, IMP, END_BYTE};
const byte cpx[] = {0xe0, IMM, 0xe4, ZP, 0xec, ABS, END_BYTE};
const byte sbc[] = {0xe9, IMM, 0xe1, INDX, 0xf1, INDY, 0xf5, ZPX, 0xfd, ABSX, 0xf9, ABSY, 0xe5, ZP, 0xed, ABS, END_BYTE};
const byte inc[] = {0xf6, ZPX, 0xfe, ABSX, 0xe6, ZP, 0xee, ABS, END_BYTE};
const byte inx[] = {0xe8, IMP, END_BYTE};
const byte nop[] = {0xea, IMP, END_BYTE};
const byte beq[] = {0xf0, REL, END_BYTE};
const byte sed[] = {0xf8, IMP, END_BYTE};

// Undocumented/Illegal Opcodes (NMOS 6502 only)
// names/information taken from http://www.oxyron.de/html/opcodes02.html
const byte slo[] = {0x07, ZP, 0x17, ZPX, 0x03, INDX, 0x13, INDY, 0x0f, ABS, 0x1F, ABSX, 0x1B, ABSY, END_BYTE};
const byte rla[] = {0x27, ZP, 0x37, ZPX, 0x23, INDX, 0x33, INDY, 0x2f, ABS, 0x3f, ABSX, 0x3b, ABSY, END_BYTE};
const byte sre[] = {0x47, ZP, 0x57, ZPX, 0x43, INDX, 0x53, INDY, 0x4f, ABS, 0x5f, ABSX, 0x5b, ABSY, END_BYTE};
const byte rra[] = {0x67, ZP, 0x77, ZPX, 0x63, INDX, 0x73, INDY, 0x6f, ABS, 0x7f, ABSX, 0x7b, ABSY, END_BYTE};
const byte sax[] = {0x87, ZP, 0x97, ZPY, 0x83, INDX, 0x8f, ABS, END_BYTE};
const byte lax[] = {0xa7, ZP, 0xb7, ZPY, 0xa3, INDX, 0xb3, INDY, 0xaf, ABS, 0xbf, ABSY, END_BYTE};
const byte dcp[] = {0xc7, ZP, 0xd7, ZPX, 0xc3, INDX, 0xd3, INDY, 0xcf, ABS, 0xdf, ABSX, 0xdb, ABSY, END_BYTE};
const byte isc[] = {0xe7, ZP, 0xf7, ZPX, 0xe3, INDX, 0xf3, INDY, 0xef, ABS, 0xff, ABSX, 0xfb, ABSY, END_BYTE};
const byte anc[] = {0x0b, IMM, END_BYTE}; // has duplicate at 0x2b
const byte alr[] = {0x4b, IMM, END_BYTE};
const byte arr[] = {0x6b, IMM, END_BYTE};
const byte axs[] = {0xcb, IMM, END_BYTE};
const byte las[] = {0xbb, ABSY, END_BYTE};

// unstable opcodes
const byte ahx[] = {0x93, INDY, 0x9f, ABSY, END_BYTE};
const byte shy[] = {0x9c, ABSX, END_BYTE};
const byte shx[] = {0x9e, ABSY, END_BYTE};
const byte tas[] = {0x9b, ABSY, END_BYTE};
const byte xaa[] = {0x8b, IMM, END_BYTE};

// reserved words
const std::vector<std::pair<const char *, const byte *>> mnemonics = {
    {"BRK", brk},
    {"PHP", php},
    {"BPL", bpl},
    {"CLC", clc},
    {"JSR", jsr},
    {"PLP", plp},
    {"BMI", bmi},
    {"SEC", sec},
    {"RTI", rti},
    {"PHA", pha},
    {"BVC", bvc},
    {"CLI", cli},
    {"RTS", rts},
    {"PLA", pla},
    {"BVS", bvs},
    {"SEI", sei},
    {"DEY", dey},
    {"BCC", bcc},
    {"TYA", tya},
    {"LDY", ldy},
    {"TAY", tay},
    {"BCS", bcs},
    {"CLV", clv},
    {"CPY", cpy},
    {"INY", iny},
    {"BNE", bne},
    {"CLD", cld},
    {"CPX", cpx},
    {"INX", inx},
    {"BEQ", beq},
    {"SED", sed},
    {"ORA", ora},
    {"AND", and_},
    {"EOR", eor},
    {"ADC", adc},
    {"STA", sta},
    {"LDA", lda},
    {"CMP", cmp},
    {"SBC", sbc},
    {"ASL", asl},
    {"ROL", rol},
    {"LSR", lsr},
    {"ROR", ror},
    {"TXA", txa},
    {"TXS", txs},
    {"LDX", ldx},
    {"TAX", tax},
    {"TSX", tsx},
    {"DEX", dex},
    {"NOP", nop},
    {"BIT", bit},
    {"JMP", jmp},
    {"STY", sty},
    {"STX", stx},
    {"DEC", dec},
    {"INC", inc},

    /* begin undocumented/illegal opcodes */
    {"SLO", slo},
    {"RLA", rla},
    {"SRE", sre},
    {"RRA", rra},
    {"SAX", sax},
    {"LAX", lax},
    {"DCP", dcp},
    {"ISC", isc},
    {"ANC", anc},
    {"ALR", alr},
    {"ARR", arr},
    {"AXS", axs},
    {"LAS", las},

    /* unstable instructions */
    {"AHX", ahx},
    {"SHY", shy},
    {"SHX", shx},
    {"TAS", tas},
    {"XAA", xaa}};

const char *unstablelist[] = {
    "AHX", "SHY", "SHX", "TAS", "XAA"};

const std::vector<directive> directives = {
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
    {"UNSTABLE", unstable},
};

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

const char whitesp[] = " \t\r\n:";   // treat ":" like whitespace (for labels)
const char whitesp2[] = " \t\r\n\""; // (used for filename processing)

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

int hexify(int i)
{
  if (i >= '0' && i <= '9')
  {
    return i - '0';
  }
  else if (i >= 'a' && i <= 'f')
  {
    return i - ('a' - 10);
  }
  else if (i >= 'A' && i <= 'F')
  {
    return i - ('A' - 10);
  }
  else
  {
    errmsg = NotANumber;
    return 0;
  }
}

#define eatwhitespace(str) (*str += strspn(*str, whitesp))

// find end of str, excluding any chars in whitespace
char *strend(char *str, const char *whitespace)
{
  char c;
  char *w = const_cast<char *>(whitespace);
  char *end = str + strlen(str);
  while (*w && end != str)
  {
    for (w = const_cast<char *>(whitespace), c = end[-1]; *w; w++)
    {
      if (*w == c)
      {
        end--;
        break;
      }
    }
  }
  return end;
}

// decode str into a number
// set errmsg on error
char gvline[WORDMAX];
int dependant; // set to nonzero if symbol couldn't be resolved
int getvalue(char **str)
{
  char *s, *end;
  int ret, chars, j;
  label *p;

  getword(gvline, str, 1);

  s = gvline;
  if (!*s)
  {
    errmsg = MissingOperand;
    return 0;
  }

  ret = chars = 0;
  if (*s == '$')
  { // hex---------------------
    s++;
    if (!*s)
    {
      ret = addr; // $ by itself is the PC
    }
    else
      do
      {
      hexi:
        j = hexify(*s);
        s++;
        chars++;
        ret = (ret << 4) | j;
      } while (*s);
    if (chars > 8)
      errmsg = OutOfRange;
  }
  else if (*s == '%')
  { // binary----------------------
    s++;
    do
    {
    bin:
      j = *s;
      s++;
      chars++;
      j -= '0';
      if (j > 1)
      {
        errmsg = NotANumber;
      }
      ret = (ret << 1) | j;
    } while (*s);
    if (chars > 32)
      errmsg = OutOfRange;
  }
  else if (*s == '\'')
  { // char-----------------
    s++;
    if (*s == '\\')
      s++;
    ret = *s;
    s++;
    if (*s != '\'')
      errmsg = NotANumber;
  }
  else if (*s == '"')
  { // char 2-----------------
    s++;
    if (*s == '\\')
      s++;
    ret = *s;
    s++;
    if (*s != '"')
      errmsg = NotANumber;
  }
  else if (*s >= '0' && *s <= '9')
  { // number--------------
    end = s + strlen(s) - 1;
    if (strspn(s, "0123456789") == strlen(s))
      ret = atoi(s);
    else if (*end == 'b' || *end == 'B')
    {
      *end = 0;
      goto bin;
    }
    else if (*end == 'h' || *end == 'H')
    {
      *end = 0;
      goto hexi;
    }
    else
      errmsg = NotANumber;
  }
  else
  { // label---------------
    p = findlabel(gvline);
    if (!p)
    { // label doesn't exist (yet?)
      needanotherpass = true;
      dependant = 1;
      if (lastchance)
      { // only show error once we're certain label will never exist
        errmsg = UnknownLabel;
      }
    }
    else
    {
      dependant |= !p->line;
      needanotherpass |= !p->line;
      if (p->type == LABEL || p->type == VALUE)
      {
        ret = p->value;
      }
      else if (p->type == MACRO)
      {
        errmsg = "Can't use macro in expression.";
      }
      else
      { // what else is there?
        errmsg = UnknownLabel;
      }
    }
  }
  return ret;
}

// get operator from str and advance str
int getoperator(char **str)
{
  *str += strspn(*str, whitesp); // eatwhitespace
  (*str)++;
  switch (*(*str - 1))
  {
  case '&':
    if (**str == '&')
    {
      (*str)++;
      return ANDAND;
    }
    else
      return AND;
  case '|':
    if (**str == '|')
    {
      (*str)++;
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
    if (**str == '=')
      (*str)++;
    return EQUAL;
  case '>':
    if (**str == '=')
    {
      (*str)++;
      return GREATEREQ;
    }
    else if (**str == '>')
    {
      (*str)++;
      return RIGHTSHIFT;
    }
    else
      return GREATER;
  case '<':
    if (**str == '=')
    {
      (*str)++;
      return LESSEQ;
    }
    else if (**str == '>')
    {
      (*str)++;
      return NOTEQUAL;
    }
    else if (**str == '<')
    {
      (*str)++;
      return LEFTSHIFT;
    }
    else
      return LESS;
  case '!':
    if (**str == '=')
    {
      (*str)++;
      return NOTEQUAL;
    }
    // no break
  default:
    (*str)--;
    return NOOP;
  }
}

// evaluate expression in str and advance str
int eval(char **str, int precedence)
{
  char unary;
  char *s, *s2;
  int ret, val2;
  int op;

  s = *str + strspn(*str, whitesp); // eatwhitespace
  unary = *s;
  switch (unary)
  {
  case '(':
    s++;
    ret = eval(&s, WHOLEEXP);
    s += strspn(s, whitesp); // eatwhitespace
    if (*s == ')')
      s++;
    else
      errmsg = IncompleteExp;
    break;
  case '#':
    s++;
    ret = eval(&s, WHOLEEXP);
    break;
  case '~':
    s++;
    ret = ~eval(&s, UNARY);
    break;
  case '!':
    s++;
    ret = !eval(&s, UNARY);
    break;
  case '<':
    s++;
    ret = eval(&s, UNARY) & 0xff;
    break;
  case '>':
    s++;
    ret = (eval(&s, UNARY) >> 8) & 0xff;
    break;
  case '+':
  case '-':
    // careful.. might be a +-label
    s2 = s;
    s++;
    op = dependant; // eval() is reentrant so don't mess up dependant
    val2 = needanotherpass;
    dependant = 0;
    ret = getvalue(&s2);
    if (errmsg == UnknownLabel)
      errmsg = 0;
    if (!dependant || s2 == s)
    { // found something or single + -
      s = s2;
      s2 = 0; // flag that we got something
      dependant |= op;
    }
    else
    { // not a label after all..
      dependant = op;
      needanotherpass = val2;
    }
    if (s2)
    { // if it wasn't a +-label
      ret = eval(&s, UNARY);
      if (unary == '-')
        ret = -ret;
    }
    break;
  default:
    ret = getvalue(&s);
  }

  do
  {
    *str = s;
    op = getoperator(&s);
    if (precedence < prec[op])
    {
      val2 = eval(&s, prec[op]);
      if (!dependant)
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
          if (!val2)
            errmsg = DivZero;
          else
            ret /= val2;
          break;
        case MOD:
          if (!val2)
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
        }
      else
        ret = 0;
    }
  } while (precedence < prec[op] && !errmsg);
  return ret;
}

// copy next word from src into dst and advance src
// mcheck=1 to crop mathy stuff (0 for filenames,etc)
void getword(char *dst, char **src, int mcheck)
{
  *src += strspn(*src, whitesp); // eatwhitespace
  strncpy(dst, *src, WORDMAX - 1);
  dst[WORDMAX - 1] = 0;
  strtok(dst, whitesp); // no trailing whitespace
  if (mcheck)
    strtok(dst, mathy);
  *src += strlen(dst);
  if (**src == ':')
    (*src)++; // cheesy fix for rept/macro listing
}

FILE *getfile(const char *fname, const char *args)
{
  std::string filename(fname);
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

// get word in src, advance src, and return reserved label*
label *getreserved(char **src)
{
  char dst[WORDMAX];
  char upp[WORDMAX];

  *src += strspn(*src, whitesp); // eatwhitespace
  if (**src == '=')
  { // special '=' reserved word
    upp[0] = '=';
    upp[1] = 0;
    (*src)++;
  }
  else
  {
    if (**src == '.') // reserved words can start with "."
      (*src)++;
    getword(dst, src, 1);
    strcpy(upp, dst);
    my_strupr(upp);
  }

  label *p = findlabel(upp); // case insensitive reserved word
  if (p == nullptr)
    p = findlabel(dst); // or case sensitive macro

  if (p != nullptr)
  {
    if (p->type == MACRO)
    {
      if (p->pass != pass)
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

// copy word to dst, advance src
// return true if it looks like a label
int getlabel(char *dst, char **src)
{
  char *s;
  char c;

  getword(dst, src, 1);
  if (*dst == '$' && !dst[1]) // '$' label
    return 1;

  s = dst; // +label, -label
  c = *s;
  if (c == '+' || c == '-')
  {
    do
      s++;
    while (*s == c);
    if (!*s) // just ++.. or --.., no text
      return 1;
  }
  c = *s;
  if (c == LOCALCHAR || c == '_' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
  { // label can start with these
    return 1;
  }
  else
  {
    errmsg = Illegal; // fucked up instruction
    return 0;
  }
}

// Expand all equates from src into dst, and remove comment
// returns a pointer to the comment in src or null.
// CRIPES what a mess...
char *expandline(char *dst, char *src)
{
  char *start;
  char *comment = nullptr;
  char c, c2;

  char upp[WORDMAX];

  bool def_skip = false;
  do
  {
    c = *src;
    if (c == '$' || (c >= '0' && c <= '9'))
    { // read past numbers (could be mistaken for a symbol, i.e. $BEEF)
      do
      {
        *dst = c;
        src++;
        dst++;
        c = *src;
      } while ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'H') || (c >= 'a' && c <= 'h'));
      c = 1; // don't terminate yet
    }
    else if (c == '"' || c == '\'')
    { // read past quotes
      *dst = c;
      dst++;
      src++;
      do
      {
        *dst = c2 = *src;
        if (c2 == '\\')
        {
          dst++;
          src++;
          *dst = *src;
        }
        dst++;
        src++;
      } while (c2 && c2 != c);
      c = c2;
    }
    else if (c == '_' || c == '.' || c == LOCALCHAR || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
    { // symbol
      start = src;
      do
      { // scan to end of symbol
        src++;
        c = *src;
      } while (c == '_' || c == '.' || c == LOCALCHAR || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));

      *src = '\0'; // terminate @ end of word (temporarily)

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
        strcpy(upp, start + (*start == '.'));
        my_strupr(upp);
        if (!strcmp(upp, "IFDEF") || !strcmp(upp, "IFNDEF"))
        {
          def_skip = true;
        }
        else
        {
          p = findlabel(start);
        }
      }

      if (p != nullptr)
      {
        if (p->type != EQUATE || p->pass != pass) // equates MUST be defined before being used otherwise they will be expanded in their own definition
          p = nullptr;                            // i.e. (label equ whatever) gets translated to (whatever equ whatever)
        else
        {
          if (p->used)
          {
            p = nullptr;
            errmsg = RecurseEQU;
          }
        }
      }
      if (p != nullptr)
      {
        p->used = true;
        expandline(dst, p->line);
        p->used = false;
      }
      else
      {
        strcpy(dst, start);
      }
      dst += strlen(dst);
      *src = c;
    }
    else
    {
      if (c == ';')
      { // comment
        c = '\0';
        comment = src;
      }
      *dst = c;
      dst++;
      src++;
    }
  } while (c != '\0');

  return comment;
}

int eatchar(char **str, char c)
{
  if (c)
  {
    *str += strspn(*str, whitesp); // eatwhitespace
    if (**str == c)
    {
      (*str)++;
      return 1;
    }
    else
      return 0;
  }
  return 1;
}

// reverse string
void reverse(char *dst, char *src)
{
  dst += strlen(src);
  *dst = 0;
  while (*src)
    *(--dst) = *(src++);
}

// ===========================================================================================================
/* [freem addition(imported code from asm6_sonder.c)] */
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
  // bank files: <output>.bank#hex.nl

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
            ((l->type == EQUATE || l->type == VALUE) && strlen(l->name) > 1)) &&
        l->value < 0x10000)
    {
      sprintf(str, "$%04X#%s#\n", (unsigned int)l->value, l->name);
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

    if ((l->type == LABEL || ((l->type == EQUATE || l->type == VALUE) && strlen(l->name) > 1)) &&
        l->name[0] != '-' && l->name[0] != '+') // no anonymous labels
    {
      sprintf(str, "%s = 0x%04X\n", l->name, (unsigned int)l->value);
      fwrite((const void *)str, 1, strlen(str), mainfile);
    }
  }

  fclose(mainfile);
}

bool comparelabels(label *&a, label *&b)
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
  return strcmp(a->name, b->name) < 0;
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
  std::string filename(outputfilename);

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
      if (l->pos < 16) // TODO - this thows out labels?
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
      sprintf(str, "P:%04X:%s", (unsigned int)(l->pos - 16), l->name);
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
        sprintf(str, "R:%04X:%s\n", (unsigned int)l->value, l->name);
      }
      else if (l->value >= 0x6000 && l->value < 0x8000)
      {
        // Assume save/work RAM ($6000-$7FFF), dump as both. (not the best solution - maybe an option?)
        sprintf(str, "S:%04X:%s\n", (unsigned int)l->value - 0x6000, l->name);
        sprintf(str, "W:%04X:%s\n", (unsigned int)l->value - 0x6000, l->name);
      }
      else
      {
        // Assume a global register for everything else (e.g $8000 for mapper control, etc.)
        sprintf(str, "G:%04X:%s\n", (unsigned int)l->value, l->name);
      }
      fwrite((const void *)str, 1, strlen(str), outfile);
    }
  }

  fclose(outfile);
}

// local:
//  false: if label starts with LOCALCHAR, make it local, otherwise it's global
//  true: force label to be local (used for macros)
void addlabel(char *word, bool local)
{
  char c = *word;
  label *p = findlabel(word);
  if (p && local && !p->scope && p->type != VALUE) // if it's global and we're local
    p = 0;                                         // pretend we didn't see it (local label overrides global of the same name)

  // global labels advance scope
  if (c != LOCALCHAR && !local)
  {
    scope = nextscope++;
  }

  if (!p)
  { // new label
    labelhere = newlabel();
    if (!labelhere->name) // name already set if it's a duplicate
      labelhere->name = my_strdup(word);
    labelhere->type = LABEL; // assume it's a label.. could mutate into something else later
    labelhere->pass = pass;
    labelhere->value = addr;
    labelhere->line = reinterpret_cast<char *>(addr >= 0 ? true_ptr : nullptr);
    labelhere->used = false;

    // [freem edit (from asm6_sonder.c)]
    labelhere->pos = filepos;

    // [freem addition]
    labelhere->ignorenl = nonl;

    if (c == LOCALCHAR || local)
    { // local
      labelhere->scope = scope;
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
    if (p->pass == pass && c != '-')
    { // if this label already encountered
      if (p->type == VALUE)
        return;
      else
        errmsg = LabelDefined;
    }
    else
    { // first time seen on this pass or (-) label
      p->pass = pass;
      if (p->type == LABEL)
      {
        if (p->value != addr && c != '-')
        {
          needanotherpass = true; // label position is still moving around
          if (lastchance)
            errmsg = BadAddr;
        }
        p->value = addr;
        p->pos = filepos;
        p->line = reinterpret_cast<char *>(addr >= 0 ? true_ptr : nullptr);
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
  for (const auto &word : mnemonics)
  {                        // opcodes first
    findlabel(word.first); // must call findlabel before using newlabel
    p = newlabel();
    p->name = word.first;
    p->value = (ptrdiff_t)opcode;
    p->line = const_cast<char *>(reinterpret_cast<const char *>(word.second));
    p->type = RESERVED;
  }

  for (const auto &directive : directives)
  { // other reserved words now
    findlabel(const_cast<char *>(directive.name));
    p = newlabel();
    p->name = directive.name;
    p->value = (ptrdiff_t)directive.func;
    p->type = RESERVED;
  }
  lastlabel = p;
}

void initcomments()
{
  comments = std::vector<comment *>();
}

void addcomment(char *text)
{
  static unsigned oldpass = 0;
  int commentcount = comments.size();
  if (oldpass != pass)
  {
    oldpass = pass;
    commentcount = 0;
  }

  text++; // ignore the leading ";"

  if (lastcommentpos == filepos)
  {
    // Append comment to the previous comment, since they are for the same address
    comment *c = comments.at(commentcount - 1);

    c->text += text;
  }
  else
  {
    // Add a new comment
    comment *c = new comment;
    c->pos = filepos;
    c->text = std::string(text);

    // Get rid of last character (newline \n)
    c->text.pop_back();

    comments.at(commentcount) = c;
    commentcount++;

    lastcommentpos = filepos;
  }
}

// find label with this label_name
// returns label* if found (and scope/etc is correct), returns nullptr if nothing found
// if label_name wasn't found, findindex points to where label_name would be inserted (label_name<labellist[findindex])
// if label_name was found but with wrong scope/whatever, findcmp=0.
// don't call if list is empty!
int findcmp;   // (these are used by newlabel)
int findindex; // .
label *findlabel(const char *label_name)
{

  int head = 0;
  int tail = labellist.size() - 1;
  findindex = labellist.size() / 2;
  do
  { // assume list isn't empty
    findcmp = strcmp(label_name, labellist.at(findindex)->name);
    if (findcmp < 0)
    {
      tail = findindex - 1;
      findindex -= (tail - head) / 2 + 1;
    }
    else if (findcmp > 0)
    {
      head = findindex + 1;
      findindex += (tail - head) / 2 + 1;
    }
  } while (findcmp && (tail - head) >= 0);
  if (findcmp)
  {
    if (findcmp < 0)
      findindex++; // position findindex so the label it points to needs to shift right
    return nullptr;
  }
  label *p = labellist[findindex];

  // check scope: label only visible if p.scope=(scope or 0)
  label *global = nullptr;
  if (*label_name == '+')
  { // forward labels need special treatment :P
    do
    {
      if (p->pass != pass)
      {
        if (!p->scope)
          global = p;
        if (p->scope == scope)
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
      if (p->scope == scope)
        return p;
      p = p->link;
    } while (p);
  }
  return global; // return global label only if no locals were found
}

// make new empty label and add it to list using result from last findlabel
// ONLY use after calling findlabel
label *newlabel()
{
  label *p = (label *)my_malloc(sizeof(label));
  p->link = nullptr;
  p->scope = 0;
  p->name = 0;

  if (!findcmp)
  {                                       // new label with same name
    p->name = labellist[findindex]->name; // share old name
    p->link = labellist[findindex];
    labellist[findindex] = p;
    return p;
  }

  labellist.insert(labellist.begin() + findindex, p);
  return p;
}

// why's this line here?
// ==============================================================================

void showerror(char *errsrc, int errline)
{
  error = true;
  fprintf(stderr, "%s(%i): %s\n", errsrc, errline, errmsg);

  if (!listerr) // only list the first error for this line
    listerr = errmsg;
}

// process the open file f
char fileline[LINEMAX];
void processfile(FILE *f, char *name)
{
  static int nest = 0;
  int nline = 0;
  int eof;
  nest++; // count nested include()s
  do
  {
    nline++;
    eof = !fgets(fileline, LINEMAX, f);
    if (!eof)
      processline(fileline, name, nline);
  } while (!eof);
  nest--;
  nline--;
  if (!nest)
  { // if main source file (not included)
    errmsg = 0;
    if (iflevel != 0)
      errmsg = NoENDIF;
    if (reptcount)
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
void processline(char *src, char *errsrc, int errline)
{
  char line[LINEMAX]; // expanded line
  char word[WORDMAX];
  char *s, *s2, *comment;
  char *endmac;
  label *p;

  errmsg = 0;
  comment = expandline(line, src);
  if (!macrolevel || verboselisting)
    listline(line, comment);

  s = line;
  if (errmsg)
  { // expandline error?
    showerror(errsrc, errline);
  }
  else
  {
    do // ????????
    {
      if (makemacro)
      { // we're inside a macro definition
        p = getreserved(&s);
        errmsg = endmac = 0;
        if (!p)
        { // skip over label if there is one, we're looking for "ENDM"
          endmac = s;
          p = getreserved(&s);
        }
        if (p)
          if (p->value == (ptrdiff_t)endm)
          {
            comment = nullptr;
            if (endmac)
            {
              endmac[0] = '\n';
              endmac[1] = 0; // hide "ENDM" in case of "label: ENDM"
            }
            else
              makemacro = 0; // don't bother adding the last line
          }
        if (makemacro && makemacro != true_ptr)
        {
          if (comment != nullptr)
            strcat(line, comment); // keep comment for listing
          *makemacro = my_malloc(strlen(line) + sizeof(char *) + 1);
          makemacro = (char **)*makemacro;
          *makemacro = 0;
          strcpy((char *)&makemacro[1], line);
        }
        if (p)
          if (p->value == (ptrdiff_t)endm)
            makemacro = 0;
        break;
      }

      if (reptcount)
      { // REPT definition is in progress?
        p = getreserved(&s);
        errmsg = endmac = 0;
        if (!p)
        {
          endmac = s;
          p = getreserved(&s);
        }
        if (p)
        {
          if (p->value == (ptrdiff_t)rept)
          {
            ++reptcount; // keep track of how many ENDR's are needed to finish
          }
          else if (p->value == (ptrdiff_t)endr)
          {
            if (!(--reptcount))
            {
              comment = nullptr;
              if (endmac)
              {
                endmac[0] = '\n'; // hide "ENDR" in case of "label: ENDR"
                endmac[1] = 0;
              }
            }
          }
        }

        if (reptcount || endmac)
        { // add this line to REPT body
          if (comment != nullptr)
            strcat(line, comment); // keep comment for listing
          *makerept = my_malloc(strlen(line) + sizeof(char *) + 1);
          makerept = (char **)*makerept;
          *makerept = 0;
          strcpy((char *)&makerept[1], line);
        }
        if (!reptcount)
        { // end of REPT, expand the whole thing right now
          expandrept(errline, errsrc);
        }
        break;
      }

      labelhere = 0; // for non-label symbol definitions (EQU,=,etc)
      s2 = s;
      p = getreserved(&s);
      errmsg = 0;

      if (skipline[iflevel])
      { // conditional assembly.. no code generation
        if (!p)
        { // it was a label... ignore it and move on
          p = getreserved(&s);
          if (!p)
            break;
        }
        if (p->value != (ptrdiff_t)else_ &&
            p->value != (ptrdiff_t)elseif &&
            p->value != (ptrdiff_t)endif &&
            p->value != (ptrdiff_t)if_ &&
            p->value != (ptrdiff_t)ifdef &&
            p->value != (ptrdiff_t)ifndef)
          break; // WHY
      }
      if (!p)
      { // maybe a label?
        if (getlabel(word, &s2))
          addlabel(word, macrolevel);
        if (errmsg)
        {
          showerror(errsrc, errline);
          return;
        }

        p = getreserved(&s);
      }
      if (p)
      {
        if (p->type == MACRO)
          expandmacro(p, &s, errline, errsrc);
        else
          ((icfn)p->value)(p, &s);
      }
      if (!errmsg)
      { // check extra garbage
        s += strspn(s, whitesp);
        if (*s)
          errmsg = "Extra characters on line.";
      }
      if (errmsg)
      {
        showerror(errsrc, errline);
      }
    } while (0);
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

// -------------------------------------------------------------------------- //

int main(int argc, char **argv)
{
  char str[512];

  char *nameptr;
  label *p;
  FILE *f;

  if (argc < 2)
  {
    showhelp();
    return EXIT_FAILURE;
  }
  initlabels();
  initcomments();

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
          if (!findlabel(&argv[i][2]))
          {
            p = newlabel();
            p->name = my_strdup(&argv[i][2]);
            p->type = VALUE;
            p->value = 1;
            p->line = static_cast<char *>(true_ptr);
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
  if (!f)
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
  p = 0;
  do
  {
    filepos = 0;
    pass++;
    if (pass == MAXPASSES || (p == lastlabel))
      lastchance = true; // give up on too many tries or no progress made
    if (lastchance)
      message("last try..\n");
    else
      message("pass %i..\n", pass);
    needanotherpass = false;
    skipline[0] = 0;
    scope = 1;
    nextscope = 2;
    defaultfiller = DEFAULTFILLER; // reset filler value
    addr = NOORIGIN;               // undefine origin
    p = lastlabel;
    nameptr = inputfilename;
    include(0, &nameptr); // start assembling srcfile
    if (errmsg)
    {
      // todo - shouldn't this set error?
      fputs(errmsg, stderr); // bad inputfile??
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
  if (listfile)
    listline(0, 0);

  // [freem addition] only generate labelfiles if asked
  if (genfceuxnl)
    export_labelfiles();
  if (genlua)
    export_lua();
  if (genmesenlabels)
    export_mesenlabels();

  return error ? EXIT_FAILURE : 0;
}

#define LISTMAX 8 // number of output bytes to show in listing
byte listbuff[LISTMAX];
int listcount;
void output(byte *p, int size, int cdlflag)
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
    if (oldpass != pass)
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
  if (oldpass != pass)
  {
    oldpass = pass;
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

// Outputs integer as little-endian. See readme.txt for proper usage.
static void output_le(int n, int size, int cdlflag)
{
  byte b[2];
  b[0] = n;
  b[1] = n >> 8;
  output(b, size, cdlflag);
}

// end listing when src=0
char srcbuff[LINEMAX];
void listline(char *src, char *comment)
{
  static unsigned oldpass = 0;
  if (!listfilename)
    return;
  if (oldpass != pass)
  { // new pass = new listfile
    oldpass = pass;
    if (listfile)
      fclose(listfile);
    listfile = fopen(listfilename, "w");
    if (!listfile)
    {
      listfilename = 0; // stop trying
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
    int i;
    for (i = 0; i < listcount && i < LISTMAX; i++)
      fprintf(listfile, " %02X", (int)listbuff[i]);
    for (; i < LISTMAX; i++)
      fprintf(listfile, "   ");
    fputs(listcount > LISTMAX ? ".. " : "   ", listfile);
    fputs(srcbuff, listfile);
    if (listerr)
    {
      fprintf(listfile, "*** %s\n", listerr);
      listerr = 0;
    }
  }
  listcount = 0;
  if (src)
  {
    if (addr < 0)
      fprintf(listfile, "\t ");
    else
      fprintf(listfile, "%05X", (int)addr);
    strcpy(srcbuff, src); // make a copy of the original source line
    if (comment != nullptr)
    {
      strcat(srcbuff, comment);
      if (genmesenlabels && filepos > 0 && addr < 0x10000)
      {
        // save this comment - needed for export
        addcomment(comment);
      }
    }
  }
  else
  {
    fclose(listfile);
    message("%s written.\n", listfilename);
  }
}

// Directives:
// ------------------------------------------------------
// <directive>(label *id, char **next)
//
//  id=reserved word
//  **next=source line (ptr gets moved past directive on exit)
// ------------------------------------------------------
void equ(label *id, char **next)
{
  char str[LINEMAX];
  char *s = *next;
  if (!labelhere)
    errmsg = NeedName; // EQU without a name
  else
  {
    if (labelhere->type == LABEL)
    {                                       // new EQU.. good
      reverse(str, s + strspn(s, whitesp)); // eat whitesp off both ends
      reverse(s, str + strspn(str, whitesp));
      if (*s)
      {
        labelhere->line = my_strdup(s);
        labelhere->type = EQUATE;
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
    *s = 0; // end line
  }
}

void equal(label *id, char **next)
{
  if (!labelhere)      // labelhere=index+1
    errmsg = NeedName; // (=) without a name
  else
  {
    labelhere->type = VALUE;
    dependant = 0;
    labelhere->value = eval(next, WHOLEEXP);
    labelhere->line = reinterpret_cast<char *>(!dependant ? true_ptr : nullptr);
  }
}

void base(label *id, char **next)
{
  int val;
  dependant = 0;
  val = eval(next, WHOLEEXP);
  if (!dependant && !errmsg)
    addr = val;
  else
    addr = NOORIGIN; // undefine origin
}

// nothing to do (empty line)
void nothing(label *id, char **next)
{
}

void include(label *id, char **next)
{
  char *np = *next;
  FILE *f = getfile(np, "r+"); // read as text, the + makes recursion not possible
  if (f == nullptr)
  {
    errmsg = CantOpen;
    error = true;
  }
  else
  {
    processfile(f, np);
    fclose(f);
    errmsg = nullptr; // let `main` know file was ok
  }
  *next = np + strlen(np); // need to play safe because this could be the main srcfile
}

void incbin(label *id, char **next)
{
  FILE *f = getfile(*next, "rb");
  if (f == nullptr)
  {
    errmsg = CantOpen;
    return;
  }

  fseek(f, 0, SEEK_END);
  int filesize = ftell(f);

  int seekpos = 0;
  if (eatchar(next, ','))
    seekpos = eval(next, WHOLEEXP);
  if (!errmsg && !dependant)
    if (seekpos < 0 || seekpos > filesize)
      errmsg = SeekOutOfRange;
  if (errmsg)
  {
    if (f)
      fclose(f);
    return;
  }
  fseek(f, seekpos, SEEK_SET);
  // get size:
  int bytesleft;
  if (eatchar(next, ','))
  {
    bytesleft = eval(next, WHOLEEXP);
    if (!errmsg && !dependant)
      if (bytesleft < 0 || bytesleft > (filesize - seekpos))
        errmsg = BadIncbinSize;
    if (errmsg)
    {
      if (f)
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

  if (f)
    fclose(f);

  *next += strlen(*next);
}

void hex(label *id, char **next)
{
  char buff[LINEMAX];
  char *src;
  int dst;
  char c1, c2;
  getword(buff, next, 0);
  if (!*buff)
    errmsg = MissingOperand;
  else
    do
    {
      src = buff;
      dst = 0;
      do
      {
        c1 = hexify(*src);
        src++;
        if (*src)
        {
          c2 = hexify(*src);
          src++;
        }
        else
        { // deal with odd number of chars
          c2 = c1;
          c1 = 0;
        }
        buff[dst++] = (c1 << 4) + c2;
      } while (*src);
      output((byte *)buff, dst, DATA);
      getword(buff, next, 0);
    } while (*buff);
}

void dw(label *id, char **next)
{
  int val;
  do
  {
    val = eval(next, WHOLEEXP);
    if (!errmsg)
    {
      if (val > 65535 || val < -65536)
        errmsg = OutOfRange;
      else
        output_le(val, 2, DATA);
    }
  } while (!errmsg && eatchar(next, ','));
}

void dl(label *id, char **next)
{
  byte val;
  do
  {
    val = eval(next, WHOLEEXP) & 0xff;
    if (!errmsg)
      output(&val, 1, DATA);
  } while (!errmsg && eatchar(next, ','));
}

void dh(label *id, char **next)
{
  byte val;
  do
  {
    val = eval(next, WHOLEEXP) >> 8;
    if (!errmsg)
      output(&val, 1, DATA);
  } while (!errmsg && eatchar(next, ','));
}

void db(label *id, char **next)
{
  int val, val2;
  byte *s, *start;
  char c, quote;

  do
  {
    *next += strspn(*next, whitesp); // eatwhitespace
    quote = **next;
    if (quote == '"' || quote == '\'')
    { // string
      s = start = (byte *)*next + 1;
      do
      {
        c = *s;
        s++;
        if (!c)
          errmsg = IncompleteExp;
        if (c == '\\')
          s++;
      } while (!errmsg && c != quote);
      if (errmsg)
        continue;
      s--; // point to the "
      *s = '0';
      *next = (char *)s;
      val2 = eval(next, WHOLEEXP);
      if (errmsg)
        continue;
      while (start != s)
      {
        if (*start == '\\')
          start++;
        val = *start + val2;
        start++;
        output_le(val, 1, DATA);
      }
    }
    else
    {
      val = eval(next, WHOLEEXP);
      if (!errmsg)
      {
        if (val > 255 || val < -128)
          errmsg = OutOfRange;
        else
          output_le(val, 1, DATA);
      }
    }
  } while (!errmsg && eatchar(next, ','));
}

void dsw(label *id, char **next)
{
  int count;
  int val = defaultfiller;
  dependant = 0;
  count = eval(next, WHOLEEXP);
  if (dependant || (count < 0 && needanotherpass)) // unknown count! don't do anything
    count = 0;
  if (eatchar(next, ','))
    val = eval(next, WHOLEEXP);
  if (!errmsg && !dependant)
    if (val > 65535 || val < -32768 || count < 0)
      errmsg = OutOfRange;
  if (errmsg)
    return;
  while (count--)
    output_le(val, 2, DATA);
}

void filler(int count, char **next)
{
  int val = defaultfiller;
  if (dependant || (count < 0 && needanotherpass)) // unknown count! don't do anything
    count = 0;
  if (eatchar(next, ','))
    val = eval(next, WHOLEEXP);
  if (!errmsg && !dependant)
    if (val > 255 || val < -128 || count < 0 || count > 0x100000)
      errmsg = OutOfRange;
  if (errmsg)
    return;
  while (count--) // !#@$
    output_le(val, 1, NONE);
}

void dsb(label *id, char **next)
{
  int count;
  dependant = 0;
  count = eval(next, WHOLEEXP);
  filler(count, next);
}

void align(label *id, char **next)
{
  int count;
  dependant = 0;
  count = eval(next, WHOLEEXP);
  if (count >= 0)
  {
    if ((unsigned int)addr % count)
      count -= (unsigned int)addr % count;
    else
      count = 0;
  }
  else
    count = 0;
  filler(count, next);
}

void pad(label *id, char **next)
{
  int count;
  if (addr < 0)
  {
    errmsg = UndefinedPC;
  }
  else
  {
    dependant = 0;
    count = eval(next, WHOLEEXP) - addr;
    filler(count, next);
  }
}

void org(label *id, char **next)
{
  if (addr < 0)
    base(id, next); // this is the first ORG; PC hasn't been set yet
  else
    pad(id, next);
}

void opcode(label *id, char **next)
{
  char *s, *s2;
  int type, val = 0;
  int oldstate = needanotherpass;
  int forceRel = 0;

  if (!allowunstable)
  {
    for (int uns = 0; uns < 4; uns++)
    {
      if (!strcmp(id->name, unstablelist[uns]))
      {
        fatal_error("Unstable instruction \"%s\" used without calling UNSTABLE.", id->name);
      }
    }
  }

  for (byte *op = (byte *)id->line; *op != END_BYTE; op += 2)
  { // loop through all addressing modes for this instruction
    needanotherpass = oldstate;
    strcpy(tmpstr, *next);
    dependant = 0;
    errmsg = 0;
    type = op[1];
    s = tmpstr;
    if (type != IMP && type != ACC)
    { // get operand
      if (!eatchar(&s, ophead[type]))
        continue;
      val = eval(&s, WHOLEEXP);
      if (type == REL)
      {
        if (!dependant)
        {
          val -= addr + 2;
          if (val > 127 || val < -128)
          {
            needanotherpass = true; // give labels time to sort themselves out
            if (lastchance)
            {
              errmsg = "Branch out of range.";
              forceRel = 1;
            }
          }
        }
      }
      else
      {
        if (opsize[type] == 1)
        {
          if (!dependant)
          {
            if (val > 255 || val < -128)
              errmsg = OutOfRange;
          }
          else
          {
            if (type != IMM)
              continue; // default to non-ZP instruction
          }
        }
        else
        { // opsize[type]==2
          if ((val < 0 || val > 0xffff) && !dependant)
            errmsg = OutOfRange;
        }
      }
      if (errmsg && !dependant && !forceRel)
        continue;
    }

    my_strupr(s);
    s2 = const_cast<char *>(optail[type]);
    while (*s2)
    { // opcode tail should match input:
      if (!eatchar(&s, *s2))
        break;
      s2++;
    }
    s += strspn(s, whitesp);
    if (*s || *s2)
      continue;

    if (addr > 0xffff)
      errmsg = "PC out of range.";
    output(op, 1, CODE);
    output_le(val, opsize[type], CODE);
    *next += s - tmpstr;
    return;
  }
  if (!errmsg)
    errmsg = Illegal;
}

void if_(label *id, char **next)
{
  int val;
  if (iflevel >= IFNESTS - 1)
    errmsg = IfNestLimit;
  else
    iflevel++;
  dependant = 0;
  val = eval(next, WHOLEEXP);
  if (dependant || errmsg)
  { // don't process yet
    ifdone[iflevel] = 1;
    skipline[iflevel] = 1;
  }
  else
  {
    skipline[iflevel] = !val || skipline[iflevel - 1];
    ifdone[iflevel] = !skipline[iflevel];
  }
}

void ifdef(label *id, char **next)
{
  char s[WORDMAX];
  if (iflevel >= IFNESTS - 1)
    errmsg = IfNestLimit;
  else
    iflevel++;
  getlabel(s, next);
  skipline[iflevel] = !(ptrdiff_t)findlabel(s) || skipline[iflevel - 1];
  ifdone[iflevel] = !skipline[iflevel];
}

void ifndef(label *id, char **next)
{
  char s[WORDMAX];
  if (iflevel >= IFNESTS - 1)
    errmsg = IfNestLimit;
  else
    iflevel++;
  getlabel(s, next);
  skipline[iflevel] = (ptrdiff_t)findlabel(s) || skipline[iflevel - 1];
  ifdone[iflevel] = !skipline[iflevel];
}

void elseif(label *id, char **next)
{
  int val;
  if (iflevel != 0)
  {
    dependant = 0;
    val = eval(next, WHOLEEXP);
    if (!ifdone[iflevel])
    { // no previous true statements
      if (dependant || errmsg)
      { // don't process yet
        ifdone[iflevel] = 1;
        skipline[iflevel] = 1;
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

void else_(label *id, char **next)
{
  if (iflevel != 0)
    skipline[iflevel] = ifdone[iflevel] || skipline[iflevel - 1];
  else
    errmsg = "ELSE without IF.";
}

void endif(label *id, char **next)
{
  if (iflevel != 0)
    --iflevel;
  else
    errmsg = "ENDIF without IF.";
}

void endm(label *id, char **next)
{ // ENDM is handled during macro definition (see processline)
  errmsg = ExtraENDM;
}

void endr(label *id, char **next)
{ // ENDR is handled during macro definition (see processline)
  errmsg = ExtraENDR;
}

void macro(label *id, char **next)
{
  char *src;
  char word[WORDMAX];
  int params;

  labelhere = 0;
  if (getlabel(word, next))
    addlabel(word, 0);
  else
    errmsg = NeedName;

  makemacro = static_cast<char **>(true_ptr); // flag for processline to skip to ENDM
  if (errmsg)
  { // no valid macro name
    return;
  }
  else if (labelhere->type == LABEL)
  { // new macro
    labelhere->type = MACRO;
    labelhere->line = 0;
    makemacro = &labelhere->line;
    // build param list
    params = 0;
    src = *next;
    while (getlabel(word, &src))
    { // don't affect **next directly, make sure it's a good name first
      *next = src;
      *makemacro = my_malloc(strlen(word) + sizeof(char *) + 1);
      makemacro = (char **)*makemacro;
      strcpy((char *)&makemacro[1], word);
      ++params;
      eatchar(&src, ',');
    }
    errmsg = 0;                // remove getlabel's errmsg
    labelhere->value = params; // set param count
    *makemacro = 0;
  }
  else if (labelhere->type != MACRO)
  {
    errmsg = LabelDefined;
  }
  else
  { // macro was defined on a previous pass.. skip past params
    **next = 0;
  }
}

// errline=source file line number
// errsrc=source file name
void expandmacro(label *id, char **next, int errline, char *errsrc)
{
  // char argname[8];
  char macroerr[WORDMAX * 2]; // this should be enough, i hope..
  char **line;
  int linecount = 0;
  int oldscope;
  int arg, args;
  char c, c2, *s, *s2, *s3;

  if (id->used)
  {
    errmsg = RecurseMACRO;
    return;
  }

  oldscope = scope; // watch those nested macros..
  scope = nextscope++;
  macrolevel++;
  id->used = true;
  sprintf(macroerr, "%s(%i):%s", errsrc, errline, id->name);
  line = (char **)(id->line);

  // define macro params
  s = *next;
  args = id->value; // (named args)
  arg = 0;
  do
  {
    s += strspn(s, whitesp);  // eatwhitespace s=param start
    s2 = s;                   // s2=param end
    s3 = strpbrk(s2, ",'\""); // stop at param end or string definition
    if (!s3)
      s3 = strchr(s2, 0);
    c = *s3;
    if (c == '"' || c == '\'')
    { // go to end of string
      s3++;
      do
      {
        c2 = *s3;
        s3++;
        if (c2 == '\\')
          s3++;
      } while (c2 && c2 != c);
      if (!c2)
        s3--; // oops..too far
      c = *s3;
    }
    s2 = s3;
    *s2 = 0;
    if (*s)
    { // arg not empty
      //  sprintf(argname,"\\%i",arg);  // make indexed arg
      //  addlabel(argname,1);
      //  equ(0,&s);
      if (arg < args)
      { // make named arg
        addlabel((char *)&line[1], 1);
        equ(0, &s);
        line = (char **)*line; // next arg name
      }
      arg++;
    }
    *s2 = c;
    s = s2;
  } while (eatchar(&s, ','));
  *next = s;

  // make "\?" arg
  // {..}

  while (arg++ < args) // skip any unused arg names
    line = (char **)*line;

  while (line)
  {
    linecount++;
    processline((char *)&line[1], macroerr, linecount);
    line = (char **)*line;
  }
  errmsg = 0;
  scope = oldscope;
  macrolevel--;
  id->used = false;
}

int rept_loops;
char *repttext; // rept chain begins here
void rept(label *id, char **next)
{
  dependant = 0;
  rept_loops = eval(next, WHOLEEXP);
  if (dependant || errmsg || rept_loops < 0)
    rept_loops = 0;
  makerept = &repttext;
  repttext = 0;
  reptcount++; // tell processline to start storing up rept lines
}

void expandrept(int errline, char *errsrc)
{
  char macroerr[WORDMAX * 2]; // source to show in listing (this should be enough, i hope?)
  char **start, **line;
  int linecount;
  int oldscope;

  start = (char **)repttext; // first rept data
  oldscope = scope;
  macrolevel++;
  for (int i = rept_loops; i; --i)
  {
    linecount = 0;
    scope = nextscope++;
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
  scope = oldscope;
  macrolevel--;
}

int enum_saveaddr;
void enum_(label *id, char **next)
{
  int val = 0;
  dependant = 0;
  val = eval(next, WHOLEEXP);
  if (!nooutput)
    enum_saveaddr = addr;
  addr = val;
  nooutput = true;
}

void ende(label *id, char **next)
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

// [freem addition]
void ignorenl(label *id, char **next)
{
  nonl = true;
}

// [freem addition]
void endinl(label *id, char **next)
{
  if (nonl)
    nonl = false;
  else
    errmsg = ExtraENDINL;
}

void fillval(label *id, char **next)
{
  dependant = 0;
  defaultfiller = eval(next, WHOLEEXP);
}

void make_error(label *id, char **next)
{
  char *s = *next;
  reverse(tmpstr, s + strspn(s, whitesp2)); // eat whitesp, quotes off both ends
  reverse(s, tmpstr + strspn(tmpstr, whitesp2));
  errmsg = s;
  error = true;
  *next = s + strlen(s);
}

void unstable(label *id, char **next)
{
  allowunstable = true;
}

// [nicklausw] ines stuff

void inesprg(label *id, char **next)
{
  inesprg_num = eval(next, WHOLEEXP);

  if (inesprg_num < 0 || inesprg_num > 0xFF)
    errmsg = OutOfRange;

  ines_include = true;
}

void ineschr(label *id, char **next)
{
  ineschr_num = eval(next, WHOLEEXP);

  if (ineschr_num < 0 || ineschr_num > 0xFF)
    errmsg = OutOfRange;

  ines_include = true;
}

void inesmir(label *id, char **next)
{
  inesmir_num = eval(next, WHOLEEXP);

  // force 4 bits
  if (inesmir_num > 16 || inesmir_num < 0)
    errmsg = OutOfRange;

  ines_include = true;
}

void inesmap(label *id, char **next)
{
  inesmap_num = eval(next, WHOLEEXP);

  // ines 2.0 allows for some big numbers...
  if (inesmap_num > 4095 || inesmap_num < 0)
    errmsg = OutOfRange;

  ines_include = true;
}

void nes2chrram(label *id, char **next)
{
  nes2chr_num = eval(next, WHOLEEXP);

  if (nes2chr_num < 0 || nes2chr_num > 16)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}

void nes2prgram(label *id, char **next)
{
  nes2prg_num = eval(next, WHOLEEXP);

  if (nes2prg_num < 0 || nes2prg_num > 16)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}

void nes2sub(label *id, char **next)
{
  nes2sub_num = eval(next, WHOLEEXP);

  if (nes2sub_num < 0 || nes2sub_num > 16)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}

void nes2tv(label *id, char **next)
{
  nes2tv_num = eval(next, WHOLEEXP);

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

void nes2vs(label *id, char **next)
{
  nes2vs_num = 1;
  ines_include = true;
  use_nes2 = 1;
}

void nes2bram(label *id, char **next)
{
  nes2bram_num = eval(next, WHOLEEXP);

  if (nes2bram_num < 0 || nes2bram_num > 16)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}

void nes2chrbram(label *id, char **next)
{
  nes2chrbram_num = eval(next, WHOLEEXP);

  if (nes2chrbram_num < 0 || nes2chrbram_num > 16)
    errmsg = OutOfRange;

  ines_include = true;
  use_nes2 = 1;
}