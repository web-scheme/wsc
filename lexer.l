
%{
#include <stdbool.h>

#include "runtime/types.h"
#include "parser.tab.h"

static bool  parse_integer(char const* text, int base);
static bool parse_rational(char const* text, int base);
static bool  parse_real_10(char const* text);

typedef double (*strtod_fn_t)(char const*    , char const**       , int);
static double  strtod_integer(char const* str, char const** endptr, int base);
static double strtod_rational(char const* str, char const** endptr, int base);
static double  strtod_real_10(char const* str, char const** endptr, int base_unused);
static sch_object_t parse_complex(char const* text, int base, strtod_fn_t strtod_fn);
%}

%option noyywrap
%option reentrant
%option bison-bridge
%option bison-locations

LINE_ENDING           \n|\r\n|\r
INTRALINE_WHITESPACE  [ \t]
WHITESPACE            {INTRALINE_WHITESPACE}|{LINE_ENDING}
VERTICAL_LINE         \|

/* multi-byte UTF-8 sequences, according to UTF-8 spec */
ANY_BYTE        [\x00-\xFF]
ASCII           [\x00-\x7F]
UTF8_2BYTE      [\xC0-\xDF][\x80-\xBF]
UTF8_3BYTE      [\xE0-\xEF][\x80-\xBF][\x80-\xBF]
UTF8_4BYTE      [\xF0-\xF7][\x80-\xBF][\x80-\xBF][\x80-\xBF]
UTF8_NON_ASCII  {UTF8_2BYTE}|{UTF8_3BYTE}|{UTF8_4BYTE}
UTF8            {ASCII}|{UTF8_NON_ASCII}

/* identifiers */
/* TODO: Make sure to only match letters in the Unicode general categories of
 *       Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pd, Pc, Po, Sc, Sm, Sk, So, or Co,
 *       or is U+200C or U+200D. */
LETTER                    [A-Za-z]|{UTF8_NON_ASCII}
SPECIAL_INITIAL           [!$%&*/:<=>?^_~]
INITIAL                   {LETTER}|{SPECIAL_INITIAL}
DIGIT                     [0-9]
HEX_DIGIT                 {DIGIT}|[A-Fa-f]
EXPLICIT_SIGN             [+\-]
SPECIAL_SUBSEQUENT        {EXPLICIT_SIGN}|\.|@
MNEMONIC_ESCAPE           [\a\b\t\n\r]
HEX_SCALAR_VALUE          {HEX_DIGIT}+
INLINE_HEX_ESCAPE         \\x{HEX_SCALAR_VALUE};
SUBSEQUENT                {INITIAL}|{DIGIT}|{SPECIAL_SUBSEQUENT}
SIGN_SUBSEQUENT           {INITIAL}|{EXPLICIT_SIGN}|@
DOT_SUBSEQUENT            {SIGN_SUBSEQUENT}|\.
SYMBOL_ELEMENT            [^|\\]|{INLINE_HEX_ESCAPE}|{MNEMONIC_ESCAPE}|\\{VERTICAL_LINE}
SIMPLE_IDENTIFIER         {INITIAL}{SUBSEQUENT}*
VERTICAL_LINE_IDENTIFIER  {VERTICAL_LINE}{SYMBOL_ELEMENT}*{VERTICAL_LINE}
PECULIAR_IDENTIFIER       {EXPLICIT_SIGN}|{EXPLICIT_SIGN}{SIGN_SUBSEQUENT}{SUBSEQUENT}*|{EXPLICIT_SIGN}\.{DOT_SUBSEQUENT}{SUBSEQUENT}*|\.{DOT_SUBSEQUENT}{SUBSEQUENT}*

/* numbers */

EXACTNESS        \#i|\#e
SIGN             {EXPLICIT_SIGN}?
SUFFIX           (e{SIGN}{DIGIT}+)?
INFNAN           {EXPLICIT_SIGN}inf\.0|{EXPLICIT_SIGN}nan\.0


RADIX_02                 \#b
RADIX_08                 \#o
RADIX_10                 \#d
RADIX_16                 \#x
PREFIX_02                {RADIX_02}{EXACTNESS}?|{EXACTNESS}{RADIX_02}
PREFIX_08                {RADIX_08}{EXACTNESS}?|{EXACTNESS}{RADIX_08}
PREFIX_10                {RADIX_10}?{EXACTNESS}?|{EXACTNESS}{RADIX_10}
PREFIX_16                {RADIX_16}{EXACTNESS}?|{EXACTNESS}{RADIX_16}
DIGIT_02                 [01]
DIGIT_08                 [0-7]
DIGIT_10                 {DIGIT}
DIGIT_16                 {HEX_DIGIT}
UINTEGER_02              {DIGIT_02}+
UINTEGER_08              {DIGIT_08}+
UINTEGER_10              {DIGIT_10}+
UINTEGER_16              {DIGIT_16}+
INTEGER_02               {SIGN}{UINTEGER_02}
INTEGER_08               {SIGN}{UINTEGER_08}
INTEGER_10               {SIGN}{UINTEGER_10}
INTEGER_16               {SIGN}{UINTEGER_16}
URATIONAL_02             {UINTEGER_02}\/{UINTEGER_02}
URATIONAL_08             {UINTEGER_08}\/{UINTEGER_08}
URATIONAL_10             {UINTEGER_10}\/{UINTEGER_10}
URATIONAL_16             {UINTEGER_16}\/{UINTEGER_16}
RATIONAL_02              {SIGN}{URATIONAL_02}
RATIONAL_08              {SIGN}{URATIONAL_08}
RATIONAL_10              {SIGN}{URATIONAL_10}
RATIONAL_16              {SIGN}{URATIONAL_16}
DECIMAL_10               {UINTEGER_10}{SUFFIX}|\.{UINTEGER_10}{SUFFIX}|{UINTEGER_10}\.{DIGIT_10}*{SUFFIX}
REAL_10                  {SIGN}{DECIMAL_10}
COMPLEX_INTEGER_02       {INTEGER_02}@{INTEGER_02}|{INTEGER_02}{EXPLICIT_SIGN}{UINTEGER_02}i|{INTEGER_02}{EXPLICIT_SIGN}i|{INTEGER_02}{INFNAN}i|{EXPLICIT_SIGN}{UINTEGER_02}i
COMPLEX_INTEGER_08       {INTEGER_08}@{INTEGER_08}|{INTEGER_08}{EXPLICIT_SIGN}{UINTEGER_08}i|{INTEGER_08}{EXPLICIT_SIGN}i|{INTEGER_08}{INFNAN}i|{EXPLICIT_SIGN}{UINTEGER_08}i
COMPLEX_INTEGER_10       {INTEGER_10}@{INTEGER_10}|{INTEGER_10}{EXPLICIT_SIGN}{UINTEGER_10}i|{INTEGER_10}{EXPLICIT_SIGN}i|{INTEGER_10}{INFNAN}i|{EXPLICIT_SIGN}{UINTEGER_10}i|{INFNAN}i
COMPLEX_INTEGER_16       {INTEGER_16}@{INTEGER_16}|{INTEGER_16}{EXPLICIT_SIGN}{UINTEGER_16}i|{INTEGER_16}{EXPLICIT_SIGN}i|{INTEGER_16}{INFNAN}i|{EXPLICIT_SIGN}{UINTEGER_16}i
COMPLEX_RATIONAL_02      {RATIONAL_02}@{RATIONAL_02}|{RATIONAL_02}{EXPLICIT_SIGN}{URATIONAL_02}i|{RATIONAL_02}{EXPLICIT_SIGN}i|{RATIONAL_02}{INFNAN}i|{EXPLICIT_SIGN}{URATIONAL_02}i
COMPLEX_RATIONAL_08      {RATIONAL_08}@{RATIONAL_08}|{RATIONAL_08}{EXPLICIT_SIGN}{URATIONAL_08}i|{RATIONAL_08}{EXPLICIT_SIGN}i|{RATIONAL_08}{INFNAN}i|{EXPLICIT_SIGN}{URATIONAL_08}i
COMPLEX_RATIONAL_10      {RATIONAL_10}@{RATIONAL_10}|{RATIONAL_10}{EXPLICIT_SIGN}{URATIONAL_10}i|{RATIONAL_10}{EXPLICIT_SIGN}i|{RATIONAL_10}{INFNAN}i|{EXPLICIT_SIGN}{URATIONAL_10}i|{INFNAN}i
COMPLEX_RATIONAL_16      {RATIONAL_16}@{RATIONAL_16}|{RATIONAL_16}{EXPLICIT_SIGN}{URATIONAL_16}i|{RATIONAL_16}{EXPLICIT_SIGN}i|{RATIONAL_16}{INFNAN}i|{EXPLICIT_SIGN}{URATIONAL_16}i
COMPLEX_REAL_10          {REAL_10}@{REAL_10}|{REAL_10}{EXPLICIT_SIGN}{DECIMAL_10}i|{REAL_10}{EXPLICIT_SIGN}i|{REAL_10}{INFNAN}i|{EXPLICIT_SIGN}{DECIMAL_10}i|{INFNAN}i
NUM_02_INTEGER           {PREFIX_02}{INTEGER_02}
NUM_08_INTEGER           {PREFIX_08}{INTEGER_08}
NUM_10_INTEGER           {PREFIX_10}{INTEGER_10}
NUM_16_INTEGER           {PREFIX_16}{INTEGER_16}
NUM_02_RATIONAL          {PREFIX_02}{RATIONAL_02}
NUM_08_RATIONAL          {PREFIX_08}{RATIONAL_08}
NUM_10_RATIONAL          {PREFIX_10}{RATIONAL_10}
NUM_16_RATIONAL          {PREFIX_16}{RATIONAL_16}
NUM_10_REAL              {PREFIX_10}{REAL_10}
NUM_02_COMPLEX_INTEGER   {PREFIX_02}{COMPLEX_INTEGER_02}
NUM_08_COMPLEX_INTEGER   {PREFIX_08}{COMPLEX_INTEGER_08}
NUM_10_COMPLEX_INTEGER   {PREFIX_10}{COMPLEX_INTEGER_10}
NUM_16_COMPLEX_INTEGER   {PREFIX_16}{COMPLEX_INTEGER_16}
NUM_02_COMPLEX_RATIONAL  {PREFIX_02}{COMPLEX_RATIONAL_02}
NUM_08_COMPLEX_RATIONAL  {PREFIX_08}{COMPLEX_RATIONAL_08}
NUM_10_COMPLEX_RATIONAL  {PREFIX_10}{COMPLEX_RATIONAL_10}
NUM_16_COMPLEX_RATIONAL  {PREFIX_16}{COMPLEX_RATIONAL_16}
NUM_10_COMPLEX_REAL      {PREFIX_10}{COMPLEX_REAL_10}

STRING_ELEMENT  [^"\\]|{MNEMONIC_ESCAPE}|\\\"|\\\\|\\{INTRALINE_WHITESPACE}*{LINE_ENDING}{INTRALINE_WHITESPACE}*|{INLINE_HEX_ESCAPE}
STRING          \"{STRING_ELEMENT}*\"

/* state for scanning balanced nested comments */
%x NESTED_COMMENT
/* state immediately after scanning a direcive, must be followed by a delimiter */
%x POST_DIRECTIVE

%%
    /* Copied verbatim to beginning of `yylex`. */
    unsigned int comment_nesting = 0;

"("    { return           TOKEN_LPAREN; }
")"    { return           TOKEN_RPAREN; }
"#("   { return          TOKEN_LVECTOR; }
"#u8(" { return      TOKEN_LBYTEVECTOR; }
"'"    { return            TOKEN_QUOTE; }
"`"    { return       TOKEN_QUASIQUOTE; }
","    { return          TOKEN_UNQUOTE; }
",@"   { return TOKEN_UNQUOTE_SPLICING; }
"."    { return              TOKEN_DOT; }
"#;"   { return    TOKEN_DATUM_COMMENT; }

"#f"|"#false"  { yylval = SCH_FALSE; return TOKEN_DATUM; }
"#t"|"#true"   { yylval =  SCH_TRUE; return TOKEN_DATUM; }

"#\\alarm"         { yylval = SCH_CHARACTER(0x07); return TOKEN_DATUM; }
"#\\backspace"     { yylval = SCH_CHARACTER(0x08); return TOKEN_DATUM; }
"#\\delete"        { yylval = SCH_CHARACTER(0x7F); return TOKEN_DATUM; }
"#\\escape"        { yylval = SCH_CHARACTER(0x1B); return TOKEN_DATUM; }
"#\\newline"       { yylval = SCH_CHARACTER(0x0A); return TOKEN_DATUM; }
"#\\null"          { yylval = SCH_CHARACTER(0x00); return TOKEN_DATUM; }
"#\\return"        { yylval = SCH_CHARACTER(0x0D); return TOKEN_DATUM; }
"#\\space"         { yylval = SCH_CHARACTER(0x20); return TOKEN_DATUM; }
"#\\tab"           { yylval = SCH_CHARACTER(0x09); return TOKEN_DATUM; }
\#\\x[0-9A-Fa-f]+  { uint64_t point = (uint64_t)strtol(yytext + 3, NULL, 16);
                     if (!is_valid_unicode(point)) {
                         // TODO: Error?
                     }
                     yylval = SCH_CHARACTER(point);
                     return TOKEN_DATUM; }
\#\\{UTF8}         { yylval = SCH_CHARACTER(TODO);
                     return TOKEN_DATUM; }

{SIMPLE_IDENTIFIER}        { yylval = sch_symbol_make(yytext, yyleng);
                             if (!is_all_valid_unicode(yylval.data)) {
                                 /* TODO: Error? */
                             }
                             return TOKEN_DATUM; }
{VERTICAL_LINE_IDENTIFIER} { yylval = sch_symbol_parse(yytext + 1, yyleng - 2);
                             if (!is_all_valid_unicode(yylval.data)) {
                                 /* TODO: Error? */
                             }
                             return TOKEN_DATUM; }
{PECULIAR_IDENTIFIER}      { /* handle special cases `+i`, `-i`, `+inf.0`, `-inf.0`, `+nan.0`, `-nan.0` */
                             if (yyleng == 2) {
                                 if (yytext[1] == 'i') {
                                     if (yytext[0] == '+') {
                                         yylval = SCH_COMPLEX(0.0, 1.0);
                                         return TOKEN_DATUM;
                                     }
                                     if (yytext[0] == '-') {
                                         yylval = SCH_COMPLEX(0.0, -1.0);
                                         return TOKEN_DATUM;
                                     }
                                 }
                             } else if (yyleng == 6) {
                                 if (yytext[4] == '.' && yytext[5] == '0') {
                                     if (yytext[1] == 'i' && yytext[2] = 'n' && yytext[3] == 'f') {
                                         if (yytext[0] == '+') {
                                             yylval = SCH_REAL((double)(INFINITY));
                                             return TOKEN_DATUM;
                                         }
                                         if (yytext[0] == '-') {
                                             yylval = SCH_REAL((double)(-INFINITY));
                                             return TOKEN_DATUM;
                                         }
                                     } else if (yytext[1] == 'n' && yytext[2] = 'a' && yytext[3] == 'n') {
                                         if (yytext[0] == '+') {
                                             yylval = SCH_REAL((double)(NAN));
                                             return TOKEN_DATUM;
                                         }
                                         if (yytext[0] == '-') {
                                             yylval = SCH_REAL((double)(-NAN));
                                             return TOKEN_DATUM;
                                         }
                                     }
                                 }
                             }
                             yylval = sch_symbol_make(yytext, yyleng);
                             if (!is_all_valid_unicode(yylval.data)) {
                                 /* TODO: Error? */
                             }
                             return TOKEN_DATUM; }

{NUM_02_INTEGER}           { yylval =  parse_integer(yytext,  2);                   return TOKEN_DATUM; }
{NUM_08_INTEGER}           { yylval =  parse_integer(yytext,  8);                   return TOKEN_DATUM; }
{NUM_10_INTEGER}           { yylval =  parse_integer(yytext, 10);                   return TOKEN_DATUM; }
{NUM_16_INTEGER}           { yylval =  parse_integer(yytext, 16);                   return TOKEN_DATUM; }
{NUM_02_RATIONAL}          { yylval = parse_rational(yytext,  2);                   return TOKEN_DATUM; }
{NUM_08_RATIONAL}          { yylval = parse_rational(yytext,  8);                   return TOKEN_DATUM; }
{NUM_10_RATIONAL}          { yylval = parse_rational(yytext, 10);                   return TOKEN_DATUM; }
{NUM_16_RATIONAL}          { yylval = parse_rational(yytext, 16);                   return TOKEN_DATUM; }
{NUM_10_REAL}              { yylval =  parse_real_10(yytext);                       return TOKEN_DATUM; }
{NUM_02_COMPLEX_INTEGER}   { yylval =  parse_complex(yytext,  2,  &strtod_integer); return TOKEN_DATUM; }
{NUM_08_COMPLEX_INTEGER}   { yylval =  parse_complex(yytext,  8,  &strtod_integer); return TOKEN_DATUM; }
{NUM_10_COMPLEX_INTEGER}   { yylval =  parse_complex(yytext, 10,  &strtod_integer); return TOKEN_DATUM; }
{NUM_16_COMPLEX_INTEGER}   { yylval =  parse_complex(yytext, 16,  &strtod_integer); return TOKEN_DATUM; }
{NUM_02_COMPLEX_RATIONAL}  { yylval =  parse_complex(yytext,  2, &strtod_rational); return TOKEN_DATUM; }
{NUM_08_COMPLEX_RATIONAL}  { yylval =  parse_complex(yytext,  8, &strtod_rational); return TOKEN_DATUM; }
{NUM_10_COMPLEX_RATIONAL}  { yylval =  parse_complex(yytext, 10, &strtod_rational); return TOKEN_DATUM; }
{NUM_16_COMPLEX_RATIONAL}  { yylval =  parse_complex(yytext, 16, &strtod_rational); return TOKEN_DATUM; }
{NUM_10_COMPLEX_REAL}      { yylval =  parse_complex(yytext, 10,  &strtod_real_10); return TOKEN_DATUM; }

{STRING}  { yylval = parse_string(yytext + 1, yyleng - 2); }

[ \t] {
    // TODO: Ignore intraline whitespace, but increment the current column tracker.
}
{LINE_ENDING} {
    // TODO: Ignore newlines, but increment the current line tracker.
}

;[^\n\r]* {
    // TODO: Ignore line comment, but increment the current column tracker.
}
"#|"             { BEGIN(NESTED_COMMENT); }
<NESTED_COMMENT>{
  \#+\|          { ++comment_nesting; }
  \|+\#          { if (comment_nesting > 0) --comment_nesting;
                   else BEGIN(INITIAL); }
  \#+|\|+        ;
  [^#|\n\r]+     ;
  {LINE_ENDING}  ;
}

"#!fold-case"     { BEGIN(POST_DIRECTIVE); return TOKEN_DIRECTIVE_FOLD_CASE; }
"#!no-fold-case"  { BEGIN(POST_DIRECTIVE); return TOKEN_DIRECTIVE_NO_FOLD_CASE; }
  /* A directive must be followed by a delimiter, which is `[()|"; \t\n\r]|\r\n`.
   * Below are all the patterns from the initial state that start with a delimiter,
   * with the exact same actions as in the initial state.
   */
<POST_DIRECTIVE>{
  "("                        { BEGIN(INITIAL); return TOKEN_LPAREN; }
  ")"                        { BEGIN(INITIAL); return TOKEN_RPAREN; }
  {VERTICAL_LINE_IDENTIFIER} { BEGIN(INITIAL);
                               yylval = sch_symbol_parse(yytext + 1, yyleng - 2);
                               if (!is_all_valid_unicode(yylval.data)) {
                                   /* TODO: Error? */
                               }
                               return TOKEN_DATUM; }
  {STRING}                   { BEGIN(INITIAL); return  TOKEN_DATUM; }
  [ \t]                      { BEGIN(INITIAL); 
                               /* TODO: Ignore intraline whitespace,
                                *       but increment the current column tracker.*/ }
  {LINE_ENDING}              { BEGIN(INITIAL); 
                               /* TODO: Ignore newlines,
                                *       but increment the current line tracker.*/ }
  ;[^\n\r]*                  { BEGIN(INITIAL); 
                               /* TODO: Ignore comments,
                                *       but increment the current column tracker.*/ }

  {UTF8}      { /* TODO: error unexpected character after directive */;
                BEGIN(INITIAL); }
  {ANY_BYTE}  { /* TODO: error unexpected non-UTF8 byte after directive */;
                BEGIN(INITIAL); }
}

{UTF8}      { /* TODO: error unexpected character after directive */; }
{ANY_BYTE}  { /* TODO: error unexpected non-UTF8 byte after directive */; }

%%

#include "stdio.h"
void yyerror(char *s) {
  fprintf(stderr, "ERROR line %d: %s\n", yylloc.first_line, s);
}

/* Discard prefix and return whether a number is exact vs. inexact.
 *
 * Assume `textp` points to a `NULL`-terminated string that matched a number regex
 * and therefore possibly starts with a radix and/or an exactness specifier. */
static bool is_number_exact(char const** textp, bool exact_by_default) {
    char const* text = *textp;
    if (text[0] == '#') {
        switch (text[1]) {
        case 'e':
            exact_by_default = true;
            break;
        case 'i':
            exact_by_default = false;
            break;
        default:
            if (text[2] == '#')
                exact_by_default = (text[3] == 'e');
        }
        *textp = text + 2 + (int)(text[2] == '#') * 2;
    }
    return exact_by_default;
}

static sch_object_t parse_integer(char const* text, int base) {
  bool exact = is_number_exact(&text, true);
  // TODO: Something with exactness.
  return SCH_INTEGER(strtol(text, NULL, base));
}

static sch_object_t parse_rational(char const* text, int base) {
  bool exact = is_number_exact(&text, true);
  // TODO: Something with exactness.
  char const* endptr;
  long numerator = strtol(text, &endptr, base);
  long denominator = strtol(endptr + 1, NULL, base);
  return SCH_RATIONAL(numerator, denominator);
}

static sch_object_t parse_real_10(char const* text) {
  bool exact = is_number_exact(&text, false);
  // TODO: Something with exactness.
  return SCH_REAL(strtod(text, NULL));
}

static sch_object_t parse_complex(char const* text, int base, strtod_fn_t strtod_fn) {
  bool exact = is_number_exact(&text, false);
  // TODO: Something with exactness.
  char const* endptr;
  double first = strtod_fn(text, &endptr, base);
  double second, second_sign = -1.0;
  switch (endptr[0]) {
      case '+':
          second_sign = 1.0;
      case '-':
          if (yyleng == endptr - text + 2)
              second = second_sign;
          else switch (endptr[1]) {
              case 'i':
                second = second_sign * (double)INFINITY;
                break;
              case 'n':
                second = second_sign * (double)NAN;
                break;
              default:
                goto _2_parts;
          }
          break;
      case 'i':
          second = first;
          first = 0;
          break;
      default:
_2_parts:
          second = strtod_fn(endptr + 1, &endptr, base);
  }
  return SCH_COMPLEX(first, second);
}

static double strtod_integer(char const* str, char const** endptr, int base) {
  return (double)strtol(str, endptr, base);
}

static double strtod_rational(char const* str, char const** endptr, int base) {
  long numerator = strtol(str, endptr, base);
  str = *endptr;
  long denominator = strtol(str + 1, endptr, base);
  return ((double)numerator) / ((double)denominator);
}

static double strtod_real_10(char const* str, char const** endptr, int base_unused) {
    return strtod(str, endptr);
}
