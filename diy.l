%option stack
%{
#include "y.tab.h"
#include <string.h>
int decimal(char *s);
int octal(char *s);
double real(char *s);
int yyerror(char *s);
%}
%x X_COMMENT X_STRING

IDENTIFIER [A-Za-z_][A-Za-z0-9_]*

INTEGER [1-9][0-9]*                 yylval.i = decimal(yytext); return tINTEGER;
OCTAL 0[0-9]*                     yylval.i = octal(yytext); return tOCTAL;
NUMBER [0-9]*\.[0-9]+              yylval.d = real(yytext); return tNUMBER;

NUMERIC {INTEGER}|{OCTAL}|{NUMBER}

%%

==.*                ;                   /* Comments handling */
=>				    yy_push_state(X_COMMENT);
<X_COMMENT>=<       yy_pop_state();
<X_COMMENT>.|\n     ;

:=              return tASSIGN;     /* Assignement operator */

=               return tEQ;         /* Comparison operators */
"<>"            return tNE;
"<="            return tLE;
">="            return tGE;

"++"            return tINCR;
"--"            return tDECR;
!               return tDIY;

public          return tPUBLIC;     /* DIY Reserved words */
const           return tCONST;
void            return tVOID;
entry           return tENTRY;

if              return tIF;         /* Control flow */
then            return tTHEN;
else            return tELSE;
do              return tDO;
while           return tWHILE;
for             return tFOR;
in              return tIN;
upto            return tUPTO;
downto          return tDOWNTO;
step            return tSTEP;
break           return tBREAK;
continue        return tCONTINUE;

{IDENTIFIER}        yylval.s = strdup(yytext); return tIDENTIFIER;

{INTEGER}           yylval.i = decimal(yytext); return tINTEGER;
{OCTAL}             yylval.i = octal(yytext); return tOCTAL;
{NUMBER}            yylval.d = real(yytext); return tNUMBER;

\"			                yy_push_state(X_STRING); yylval.s = malloc(yyleng); *yylval.s = 0;
<X_STRING>\"			    yy_pop_state(); return tSTRING;
<X_STRING>"*n"		        strcat(yylval.s, "\n");
<X_STRING>"*r"		        strcat(yylval.s, "\r");
<X_STRING>"*t"		        strcat(yylval.s, "\t");
<X_STRING>"*0"		        strcat(yylval.s, "\0");
<X_STRING>"*("		        strcat(yylval.s, "{");
<X_STRING>"*)"		        strcat(yylval.s, "}");
<X_STRING>\*[*'"]           strcat(yylval.s, yytext+1);
<X_STRING>\*.		        yyerror("invalid escape sequence in string");
<X_STRING>.|\n              strcat(yylval.s, yytext);

[-()<>+*/%;{},.~&|?\[\]\'\"\\!]     return *yytext;

[ \t\n\r]+		                    ;

.                                   yyerror("Unknown character");

%%
int yywrap() { return 1; }

int decimal(char *text) {
    errno = 0;
    int decimal = strtol(text, 0, 10);

	if (errno == ERANGE) 
        yyerror("decimal overflow");

    return decimal;
}

int octal(char *s) {

  int i, a = 0, b = 0;

  for (i = 0; i < strlen(s); i++) {
    if (s[i] < '0' || s[i] > '9') break;
    b = b * 8 + s[i] - '0';
    if (b < a) {
      yyerror("octal overflow");
      break;
    }
    a = b;
  }
  return a;
}

double real(char *s) {

    errno = 0;
    double number = atof(s);

    if(errno == ERANGE)
        yyerror("real overflow");

    return number;
}