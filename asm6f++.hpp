#ifndef ASM6FPP_HPP
#define ASM6FPP_HPP

#include <cstdint>

#define VERSION "1.7"

#define addr firstlabel.value // '$' value
#define NOORIGIN -0x40000000  // nice even number so aligning works before origin is defined
#define INITLISTSIZE 128      // initial label list size
#define BUFFSIZE 8192         // file buffer (inputbuff, outputbuff) size
#define WORDMAX 128           // used with getword()
#define LINEMAX 2048          // plenty of room for nested equates
#define MAXPASSES 7           // number of tries before giving up
#define IFNESTS 32            // max nested .IF directives
#define DEFAULTFILLER 0       // default fill value
#define LOCALCHAR '@'         // character to define local labels

/**	
 * LABEL: known address
 * VALUE: defined with '='
 * EQUATE: defined with EQU
 * MACRO: macro
 * RESERVED: reserved word
 */
enum labeltypes
{
  LABEL,
  VALUE,
  EQUATE,
  MACRO,
  RESERVED
};

enum cdltypes
{
  NONE = 0,
  CODE = 1,
  DATA = 2
};

/**
 * ACC: Accumulator
 * IMM: Immediate
 * IND: Indirect
 * INDX: X-Indexed, Indirect
 * INDY: Indirect, Y-Inexed
 * ZPX: Zero Page, X-Indexed
 * ZPY: Zero Page, Y-Indexed
 * ABSX: Absolute, X-Indexed
 * ABSY: Absolute, Y-Indexed
 * ZP: Zero Page
 * ABS: Absolute
 * REL: Relative
 * IMP: Implied
 */
enum optypes
{
  ACC,
  IMM,
  IND,
  INDX,
  INDY,
  ZPX,
  ZPY,
  ABSX,
  ABSY,
  ZP,
  ABS,
  REL,
  IMP
};

//precedence levels
enum prectypes
{
  WHOLEEXP,
  ORORP,
  ANDANDP,
  ORP,
  XORP,
  ANDP,
  EQCOMPARE,
  COMPARE,
  SHIFT,
  PLUSMINUS,
  MULDIV,
  UNARY,
};

enum operators
{
  NOOP,
  EQUAL,
  NOTEQUAL,
  GREATER,
  GREATEREQ,
  LESS,
  LESSEQ,
  PLUS,
  MINUS,
  MUL,
  DIV,
  MOD,
  AND,
  XOR,
  OR,
  ANDAND,
  OROR,
  LEFTSHIFT,
  RIGHTSHIFT,
};

char mathy[] = "!^&|+-*/%()<>=,";

//precedence of each operator
const char prec[] = {
    WHOLEEXP,
    EQCOMPARE,
    EQCOMPARE,
    COMPARE,
    COMPARE,
    COMPARE,
    COMPARE,
    PLUSMINUS,
    PLUSMINUS,
    MULDIV,
    MULDIV,
    MULDIV,
    ANDP,
    XORP,
    ORP,
    ANDANDP,
    ORORP,
    SHIFT,
    SHIFT,
}; 

struct label
{
  const char *name; //label name

  /** 
   * value represents different things depending on the label's usage
   * label: memory address, 
   * equate: value
   * macro: param count
   * reserved word: function pointer
   */
  ptrdiff_t value;

  int pos; // Location in file; used to determine bank when exporting labels

  // TODO un-kitchensink this boi
  char *line; //for macro or equate, also used to mark unknown label
              //*next:text->*next:text->..
              //for macros, the first <value> lines hold param names
              //for opcodes (reserved), this holds opcode definitions, see initlabels

  int type;      // labeltypes enum (see above)
  int used;      // for EQU and MACRO recursion check
  int pass;      // when label was last defined
  int scope;     // where visible (0=global, nonzero=local)
  bool ignorenl; // supress this label from .nl files?
  label *link;   // labels that share the same name (local labels) are chained together
};

struct comment
{
  char *text;
  int pos;
};

struct directive
{
  const char *name;
  void (*func)(label *, char **);
};

typedef uint8_t byte;
const byte END_BYTE = -1;

// what the heck does icfn stand for?
typedef void (*icfn)(label *, char **);

label *findlabel(const char *);
void initlabels();
label *newlabel();
void getword(char *, char **, int);
int getvalue(char **);
int getoperator(char **);
int eval(char **, int);
label *getreserved(char **);
int getlabel(char *, char **);

void processline(char *, char *, int);
void listline(char *, char *);
void endlist();

// reserved word functions //

void inesprg(label *, char **);
void ineschr(label *, char **);
void inesmir(label *, char **);
void inesmap(label *, char **);

void nes2chrram(label *, char **);
void nes2prgram(label *, char **);
void nes2sub(label *, char **);
void nes2tv(label *, char **);
void nes2vs(label *, char **);
void nes2bram(label *, char **);
void nes2chrbram(label *, char **);

void opcode(label *, char **);
void org(label *, char **);
void base(label *, char **);
void pad(label *, char **);
void equ(label *, char **);
void equal(label *, char **);
void nothing(label *, char **);
void include(label *, char **);
void incbin(label *, char **);
void dw(label *, char **);
void db(label *, char **);
void dl(label *, char **);
void dh(label *, char **);
void hex(label *, char **);
void dsw(label *, char **);
void dsb(label *, char **);
void align(label *, char **);
void if_(label *, char **);
void ifdef(label *, char **);
void ifndef(label *, char **);
void elseif(label *, char **);
void else_(label *, char **);
void endif(label *, char **);
void macro(label *, char **);
void endm(label *, char **);
void endr(label *, char **);
void rept(label *, char **);
void enum_(label *, char **);
void ende(label *, char **);
void ignorenl(label *, char **);
void endinl(label *, char **);
void fillval(label *, char **);
void make_error(label *, char **);
void unstable(label *, char **);
void hunstable(label *, char **);

void expandmacro(label *, char **, int, char *);
void expandrept(int, char *);

#endif