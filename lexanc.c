/* lexanc.c         Chris Javier	ctj276		September 12 2016       */

/* Copyright (c) 2001 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "token.h"
#include "lexan.h"

//arrays for reserved words and the values for the tokens

static char* reserved[] = {"and", "div", "in", "mod", "not", "or", "array", "begin", "case", "const", "do", "downto", "else", "end", "file", "for", "function", "goto", "if", "label", "nil", "of", "packed", "procedure", "program", "record", "repeat", "set", "then", "to", "type", "until", "var", "while", "with"};

static int reservednum[] = {ANDOP, DIVOP, INOP, MODOP, NOTOP, OROP, ARRAY, BEGINBEGIN, CASE, CONST, DO, DOWNTO, ELSE, END, FILEFILE, FOR, FUNCTION, GOTO, IF, LABEL, NIL, OF, PACKED, PROCEDURE, PROGRAM, RECORD, REPEAT, SET, THEN, TO, TYPE, UNTIL, VAR, WHILE, WITH};

/* Skip blanks, whitespace, and comments*/
void skipblanks ()
  {
      int c;
      int d;
      while ((c = peekchar()) != EOF && (c == ' ' || c == '\n' || c == '\t' || c == '{' || ((c == '(') && ((d = peek2char()) == '*'))))
      {
	if (c == '{')
	{
		getchar();
		c = peekchar();
		while(c != EOF && c != '}')
		{
			getchar();
			c = peekchar();
		}
		getchar();
	}
	else if(c == '(')
	{
		getchar();
		getchar();
		c = peekchar();
		d = peek2char();
		while((c != '*' || d != ')') && (d != EOF))
		{
			getchar();
			c = peekchar();
			d = peek2char();
		}
		getchar();
		getchar();
	}
	else
	{
		getchar();
	}
      }
   }

/* Get identifiers and reserved words */
TOKEN identifier (TOKEN tok)
  {
	int c = peekchar();
	int x = 0;
	char st[16];
	while(x < 15 && c != EOF && (CHARCLASS[c] == ALPHA || CHARCLASS[c] == NUMERIC))
	{
		st[x] = getchar();
		x++;
		c = peekchar();
	}
	while((c != EOF) && (CHARCLASS[c] == ALPHA || CHARCLASS[c] == NUMERIC))
	{
		getchar();
		c = peekchar();
	}
	st[x] = '\0';
	int y = 0;
	int found = 0;
	while(found == 0 && y < 35)
	{
		if(strcmp(st, reserved[y]) == 0)
		{
			found = 1;
			if(y < 6)
			{
				tok->tokentype = OPERATOR;
				tok->whichval = reservednum[y];
				return (tok);
			}
			else
			{ 
				tok->tokentype = RESERVED;
				tok->whichval = reservednum[y] - RESERVED_BIAS;
				return (tok);
			}
		}
		else
			y++;
	}
	if(found == 0)
	{
		tok->tokentype = IDENTIFIERTOK;
		strcpy(tok->stringval, st);
		return (tok);
	}
  }

TOKEN getstring (TOKEN tok)
  {
	getchar();
	int x = 0;
	int c = peekchar();
	int d = peek2char();
	char st[16];
	while(c != EOF)
	{
		if(c == '\'')
		{
			if(d == '\'')
			{
				getchar();
			}
			else
			{
				break;
			}
		}
		if(x < 15)
		{
			st[x] = getchar();
			x++;
		}
		else
		{
			getchar();
		}
		c = peekchar();
		d = peek2char();
	}
	st[x] = '\0';
	getchar();
	tok->tokentype = STRINGTOK;
	strcpy(tok->stringval, st);
	return tok;
  }

//two helper functions for the switch statement in special

TOKEN delimtoken(int val, TOKEN tok)
{
	tok->tokentype = DELIMITER;
	tok->whichval = val - DELIMITER_BIAS;
	return(tok);
}

TOKEN opertoken(int val, TOKEN tok)
{
	tok->tokentype = OPERATOR;
	tok->whichval = val;
	return(tok);
}

TOKEN special (TOKEN tok)
  {
	char c = peekchar();
	char d = peek2char();
	switch(c)
	{
		case ',': tok = delimtoken(COMMA, tok);
			break;
		case ';': tok = delimtoken(SEMICOLON, tok);
			break;
		case '(': tok = delimtoken(LPAREN, tok);
			break;
		case ')': tok = delimtoken(RPAREN, tok);
			break;
		case '[': tok = delimtoken(LBRACKET, tok);
			break;
		case ']': tok = delimtoken(RBRACKET, tok);
			break;
		case '.': if(d == '.')
			{
				getchar();
				tok = delimtoken(DOTDOT, tok);
			}
			else
			{
				tok = opertoken(DOTOP, tok);
			}
			break;
		case '+': tok = opertoken(PLUSOP, tok);
			break;
		case '-': tok = opertoken(MINUSOP, tok);
			break;
		case '*': tok = opertoken(TIMESOP, tok);
			break;
		case '/': tok = opertoken(DIVIDEOP, tok);
			break;
		case ':': if(d == '=')
			{
				getchar();
				tok = opertoken(ASSIGNOP, tok);
			}
			else
			{
				tok = delimtoken(COLON, tok);
			}
			break;
		case '=': tok = opertoken(EQOP, tok);
			break;
		case '<': if(d == '>')
			{
				getchar();
				tok = opertoken(NEOP, tok);
			}
			else if(d == '=')
			{
				getchar();
				tok = opertoken(LEOP, tok);
			}
			else
			{
				tok = opertoken(LTOP, tok);
			}
			break;
		case '>': if(d == '=')
			{
				getchar();
				tok = opertoken(GEOP, tok);
			}
			else
			{
				tok = opertoken(GTOP, tok);
			}
			break;
		case '^': tok = opertoken(POINTEROP, tok);
			break;
		default: printf("bad symbol");
	}
	getchar();
	return tok;
  }

/* Get and convert unsigned numbers of all types. */
TOKEN number (TOKEN tok)
  { 
	long num = 0;
	int  c, d, charval;
	double realnum = 0;
	int exp = 0;
	double pexp[50];
	pexp[0] = 1;
	int sigdigs = 0;
	int i, j;
	for(i = 1; i < 50; i++)
	{
		pexp[i] = pexp[i-1] * 10;
	}
	double nexp[50];
	nexp[0] = 1;
	for(j = 1; j < 50; j++)
	{
		nexp[j] = nexp[j-1] * .1;
	}
	int leadingzeros = 1;
	int numerror = 0;
	int experror = 0;
	int realflag = 0;
	int firstnonzero = 0;
	// digits before decimal place
	while ( (c = peekchar()) != EOF && CHARCLASS[c] == NUMERIC)
	{   
		c = getchar();
		charval = (c - '0');
		if(charval != 0)
		{
			leadingzeros = 0;
		}
		num = num * 10 + charval;
		if(leadingzeros == 0)
		{
			sigdigs++;
			if(sigdigs > 8)
			{
				realnum *= 10;
			}
			else
			{
				realnum = realnum*10 + charval;
			}
		}
	}
	if(num > 2147483647 || sigdigs > 10)
	{
		numerror = 1;
	}
	firstnonzero = sigdigs - 1;
	// digits after decimal place
	c = peekchar();
	if(c == '.' && CHARCLASS[peek2char()] == NUMERIC)
	{
		getchar();
		realflag = 1;
		int x = 1;
		double realcharval;
		while( (c = peekchar()) != EOF && CHARCLASS[c] == NUMERIC)
		{
			c = getchar();
			realcharval = (c - '0');
			if(realcharval != 0)
			{
				leadingzeros = 0;
			}
			if(leadingzeros == 1)
			{
				firstnonzero--;
			}
			realcharval *= nexp[x++];
			if(leadingzeros == 0 && sigdigs <8)
			{
				realnum += realcharval;
				sigdigs++;
			}
		}
	}
	// convert realnum to correct format
	if(firstnonzero < 0)
	{
		realnum *= pexp[-firstnonzero];
	}
	else if(firstnonzero > 0)
	{
		realnum *= nexp[firstnonzero];
	}
	// digits after e or E
	c = peekchar();
	if(c == 'e' || c == 'E')
	{
		getchar();
		realflag = 1;
		int expsign = 1;
		int expleadingzeros = 1;
		c = peekchar();
		if(c == '+')
		{
			getchar();
		}
		else if(c == '-')
		{
			expsign = -1;
			getchar();
		}
		while( (c = peekchar()) != EOF && CHARCLASS[c] == NUMERIC)
		{
			c = getchar();
			charval = (c - '0');
			if(charval != 0)
			{
				expleadingzeros = 0;
			}
			if(expleadingzeros == 0)
			{
				exp = exp*10 + charval;
			}
		}
		if(exp < 0)
		{
			experror = 1;
		}
		exp *= expsign;
	}
	// put it all together
	if(realflag == 1)
	{
		exp += firstnonzero;
		if(exp > 38 || exp < -38)
		{
			experror = 1;
		}
		if(experror == 1)
		{
			printf("REAL NUM ERROR \n");
			realnum = 9999999;
		}
		else
		{
			if(exp < 0)
			{
				realnum *= nexp[-exp];
			}
			else if(exp > 0)
			{
				realnum *= pexp[exp];
			}
		}
		tok->tokentype = NUMBERTOK;
		tok->datatype = REAL;
		tok->realval = realnum;
		return (tok);
	}
	else
	{
		if(numerror == 1)
		{
			printf("INT NUM ERROR \n");
			num = 11111111;
		}
		tok->tokentype = NUMBERTOK;
		tok->datatype = INTEGER;
		tok->intval = num;
		return (tok);
	}
  }

