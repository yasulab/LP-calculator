#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>


#define TRUE 0          /* true flag */
#define FALSE -1        /* false flag */
#define MAX_CHARS 400    /*max length of chars */
#define HASHSIZE 40      /* size of hash table */
#define MAX_KW_LEN 11    /* max length of keyword */
#define NUM_KW 6         /* number of keyword */
#define isALPHABET() ('A'<=chars[cp] && chars[cp]<='Z') || ('a'<=chars[cp] && chars[cp]<='z') 

int token;              /* procedure of token                  */
double value;           /* temporary valuable for calculating  */
char chars[MAX_CHARS];  /* argument for input chars            */
char str[MAX_KW_LEN];   /* argument for deliminated string     */
int cp;                 /* pointer of chars                    */
int errorcount;         /* number of error                     */

/* constructer for resistering word */
struct list {
  char keyword[MAX_KW_LEN];
  double val;
  struct list *next;            /* next list pointer */
};

// hash table resistering word 
struct list *hashtable[HASHSIZE]; /* hash table */

int hash(char *kw);          /* return hash value of HASHSIZE from zero */
int registerKeyword(char *kw, double val); /* register keyword in hash table */
int FindKeyWord(char *kw);   /* try to find out keyword in hashtable */
void FreeKeyWord();          /* release allocated memory        */
void debug();                /* debug mode when error occurs    */
double expression();         /* implement the syntax "expression"*/
double term();               /* implement the syntax "term"     */
double primary();            /* implement the syntax "primary"  */
double figure();             /* implement the syntax "figure"   */
double character();          /* implement the syntax "character"*/
double popKeyValue(char* kw);/* pop the value using keyword from hash table  */


/* "procedure relation" */
/* number < character <'.'<'{'<'('<'+'<'-'             */
/* '*'<'/'<'%'<')'<'}'<'='< "end of line" < any error  */
enum{ Value, Character, Decimal, Lbracket, Lpar, Plus, Minus, Times, Divide, Modulo, Rpar, Rbracket, Equal, EOL, Error};

/* get chararacters from input line, and put into "chars[]"  */
int get_line(){
 int len=0;
 printf("# ");
 while((chars[len] = getchar()) != '\n'){
   len++;
 }
 if(len>0){
   chars[len]='\n';
   cp= 0;
   return TRUE;
 }else return FALSE;
}


/* get token from "chars[]" */
void get_token(){
  int i;
  while( chars[cp]==' ' ) cp++;
  if( '0'<=chars[cp] && chars[cp]<='9' ){
    token= Value; cp++;
  }else if(isALPHABET()){
    for(i=0;i<MAX_KW_LEN;i++) str[i] = '\0';
    i=0;
    while(isALPHABET()){
      str[i]=chars[cp];
      cp++; i++;
    }
    token= Character;
  }else{
    switch(chars[cp]){
    case '=': token=Equal;   cp++; break;
    case '.': token=Decimal; cp++; break;
    case '(': token=Lpar;    cp++; break;
    case ')': token=Rpar;    cp++; break;
    case '[': token=Lbracket;cp++; break;
    case ']': token=Rbracket;cp++; break;
    case '+': token=Plus;    cp++; break;
    case '-': token=Minus;   cp++; break;
    case '*': token=Times;   cp++; break;
    case '/': token=Divide;  cp++; break;
    case '%': token=Modulo;  cp++; break;
    case '\n': token=EOL;    cp++; break;
    default:  token=Error;
    }
  }

}


/* char message[]: explanation of error                */
/* int procedure:  skip chars less than this procedure */
/* int position:   explain where error is              */
void error(char message[], int procedure, int position){
  int i;
  printf("%s\n", message); 
  if(position!=-1){
    printf("%s", chars);
    for(i=0;i<position;i++){
      printf(" ");
    }
    printf("^\n");
  }
  errorcount += 1;
  // debug();
  while( token < procedure){
    get_token();
  }
}

/* expression = ['-'|'+'] term {('+'|'-') tern } */
double expression(){
  double result;
  switch(token){
    case Minus: get_token(); result= -term(); break;
    case Plus: get_token();
    default: result= term();
  }
  while( token==Plus || token==Minus ){
    int op;
    double val;
    op= token;  
    get_token();
    val= term();
    if( op==Plus ) result= result+val;
    else result= result-val;
  }
  return result;
}


/* term = primary {('*'|'/'|'%') primary } */
double term(){
 double result;
 result= primary();
 while( token==Times || token==Divide || token==Modulo ){
     int op;
     double val;
     op= token;  
     get_token();
     val= primary();
           if( op==Times ) result *= val;
     else if(op==Divide) result /= val;
     else if(op==Modulo) result = fmod(result, val);
  }
   return result;
}

/* primary = character | figure |'('expression')'|'[' expression ']' */
double primary(){
 double result;
 if( token==Value ){          // case: figure
   result= figure();
 }else if( token==Character){ // case: character
   get_token();
   result = character();
 }else if( token==Lpar ){     // case: '(' expression ')'
   get_token();
   result= expression();
   if( token==Rpar ) get_token();
   else error("error: missing ')'", Value, cp-1);
 }else if( token==Lbracket){  // case: '[' expression ']'
   get_token();
   result= floor(expression());
   if( token==Rbracket ) get_token();
   else error("error: missing ']'", Value, cp-1);
 }else{                       // case: does not match any syntax
   result= 0;
   error("error: incorrect position of symbol.", Value, cp-1);
   }
   return result;
}


/* figure = {number} ['.' {number}] */
/* number = 0|1|2|3|4|5|6|7|8|9     */
double figure(){
  int digit_cnt=0;
  int i;
  value = 0.0;
  while(token == Value){
    value= value*10+(chars[cp-1]-'0');
    get_token();
  }

  if(token == Decimal){  // case: the number is a decimal number
    get_token();
    if(token==Value){
      while(token == Value){
 value= value*10+(chars[cp-1]-'0');
 digit_cnt++;
 get_token();
      }
      for(i=0;i<digit_cnt;i++){
 value /= 10;
      }
    }else error("missing numbers after '.'", Lbracket, cp-1);
  }else if(token < Lbracket) error("error: incorrect number.", Lbracket, cp-1);
  return value;
}

/* character = ID ['=' expression]  */
/* ID = "from 'a' to 'z' and from 'A' to 'Z'" */
double character(){
  double val;
  char tmp_str[MAX_KW_LEN];
  if(token==Equal){
    strcpy(tmp_str, str);
    get_token();
    val = expression();
    registerKeyword(tmp_str, val);
    printf("%s holds %f\n", tmp_str, val);
  }else{
    if((FindKeyWord(str)) == FALSE){
      if(cp-1<0 || chars[cp] == '\n'){
 error("error: not registered word is used.", Plus, cp-1);
      }else error("error: not registered word is used.", Plus, cp-2);
    }else val = popKeyValue(str);
  }
  return val;
}


/* debug(): indicate contents of error in detail for debug */
void debug(){
 printf("\n-----debug mode-----\n");
 printf("THEN:\n");
 printf("chars[cp-1] == %c\n", chars[cp-1]);
 switch(token){
 case Character: printf("token == Character\n"); break;
 case Value: printf("token == Value\n"); break;
 case Decimal: printf("token == Decimal\n"); break;
 case Lbracket: printf("token == Lbracket\n"); break;
 case Lpar:  printf("token == Lpar\n"); break;
 case Plus:  printf("token == Plus\n"); break;
 case Minus: printf("token == Minus\n"); break;
 case Times: printf("token == Times\n"); break;
 case Divide:printf("token == Divide\n"); break;
 case Modulo:printf("token == Modulo\n"); break;
 case Error: printf("token == Error\n"); break;
 case Rpar:  printf("token == Rpar\n"); break;
 case Rbracket: printf("token == Rbracket\n"); break;
 case Equal: printf("token == Equal\n"); break;
 case EOL:   printf("token == EOL\n"); break;
 default:    printf("token is unknown\n"); break;
 }
 printf("errorcount = %d\n", errorcount);
 printf("--------------------\n\n");
}


/* hash(): return hashed key */
int hash(char *kw){
    int hashval = 0;
    while (*kw != '\0') hashval += *kw++;
    return (hashval % HASHSIZE);
}


/* register keyword in hash table */
int registerKeyword(char* kw, double val){
    int i;
    struct list *p, *q;
    int hashval;
    if ((p = (struct list *)malloc(sizeof(struct list))) == NULL) {
      fprintf(stderr, "can't allocate memory\n");
      exit(2);
    }
    
    strcpy((*p).keyword, kw);
    (*p).val = val;
    hashval = hash(kw); 
    
    if (hashtable[hashval] == NULL ||
 !strcmp(hashtable[hashval]->keyword, kw)){
      hashtable[hashval] = p;    
      p->next = NULL;            
    }else {                             /* Case: already registerd */
      q = hashtable[hashval];
      while (q->next != NULL) q = q->next;         
      q->next = p;              
      p->next = NULL;             
    }
}


/* try to find out keyword in hash table */
int FindKeyWord(char *kw){
  struct list *p;
  for (p = hashtable[hash(kw)]; p != NULL; p = p->next)
    if (!strcmp(kw, (*p).keyword)) return (TRUE);    
  return (FALSE); 
}


/* get the value of keyword from hashtable */
double popKeyValue(char* kw){
  struct list *p;
  for (p = hashtable[hash(kw)]; p != NULL; p = p->next)
    if (!strcmp(kw, (*p).keyword))
      return (*p).val;
  return FALSE;
}


/* release allocated memory */
void FreeKeyWord(void){
    int i;
    struct list *p, *q;
    for (i = 0; i < HASHSIZE; i++)
        for (p = hashtable[i]; p != NULL; ) {  
            q = p->next;                       
            free(p);            
            p = q;           
        }
}


/* initialize the argument of chars */
void initChars(){
 int i;
 for(i=0;i<MAX_CHARS;i++) chars[i] = '\0';
}



int main(){
  initChars();                // initialize the argument of chars[]
  while(get_line() != FALSE){ // if imput an empty line, shut down this program
    double result;            // the result of calculating an expression 
    get_token(); 
    errorcount= 0;
    result= expression();     /* start calculation */

      /* case: finished calculation in no problem */
    if( errorcount==0 && token==EOL ){
      printf(">> ");
      if( result<0 ){ 
 printf("-");
 result= -result;
      }
      printf("%f\n", result);

      /* case: one or some errors are found through calculation */
    }else if( errorcount==1 ){
      printf("%d error was found.\n", errorcount);
    }else if(errorcount>1){
      printf("%d errors were found.\n", errorcount);

      /* case: wasn't able to categorize error */
    }else printf("unexpected error\n");

    /* prepare for next calculation */
    initChars();
    printf("\n");
  }
  FreeKeyWord();  // release allocated memory
  printf("shut down this program...");
}

