/* Dilly Jacques, Giullya Santos
 * Dr. Montagne
 * COP3402 Fall 2023
 * HW 4
 */
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define NO_RW 14   // number of reserved names
#define NOR_SYM 13 // number of valid syms
#define CMAX 12    // maximum number of chars for a
#define DMAX 5     // max digits
#define MAX 10000  // used to be maxCode
#define MAX_SYM_TABLE_SIZE 500


typedef enum token_type {
    oddsym = 1, identsym = 2, numbersym = 3, plussym = 4, minussym = 5,
    multsym = 6, slashsym = 7, ifelsym = 8, eqlsym = 9, neqsym = 10, lessym = 11,
    leqsym = 12, gtrsym = 13, geqsym = 14, lparentsym = 15, rparentsym = 16,
    commasym = 17, semicolonsym = 18, periodsym = 19, becomessym = 20,
    beginsym = 21, endsym = 22, ifsym = 23, thensym = 24, whilesym = 25, dosym = 26,
    callsym = 27, constsym = 28, varsym = 29, procsym = 30, writesym = 31,
    readsym = 32 ,elsesym = 33
} token_type; // tokens

typedef struct reservednames {
    char name[20];
    int token;
} reservednames;

typedef struct specialSym {
    char sym;
    int token;
} specialSym;

typedef struct lexemeTable {
    char lexeme[CMAX];
    int numlexeme;
    int tokenType;
} lexemeTable;

typedef struct instruction {
	int operatorNum, l, m;

} instruction;

typedef struct symbol_table{
    int kind; // const = 1, var = 2, proc = 3
    char name[10]; // name up to 11 chars
    int val; // number (ASCII value)
    int level; // L level
    int addr; // M address
    int mark; // to indicate unavailable or deleted
} symbol_table;

// Reserved names: const, var, procedure, call, begin, end, if, then, else, while, do, read, write, odd.
reservednames rNames[NO_RW] = {{"const", constsym},{"var", varsym},{"procedure", procsym},
{"call", callsym},{"begin", beginsym},{"end", endsym},{"if", ifsym},{"then", thensym},
{"else", elsesym}, {"while", whilesym},{"do", dosym},{"read", readsym},{"write", writesym},
{"ifel", ifelsym}};

// Special syms: ‘+’, ‘-‘, ‘*’, ‘/’, ‘(‘, ‘)’, ‘=’, ’,’ , ‘.’, ‘ <’, ‘>’,  ‘;’ , ’:’ .
specialSym sym[NOR_SYM] = {{'+', plussym},{'-', minussym},{'*', multsym},{'/', slashsym},
 {'(', lparentsym},{')', rparentsym},{'=', eqlsym},{',', commasym},{'.', periodsym},
 {'<', lessym},{'>', gtrsym},{';', semicolonsym}};

 // Struct variables
lexemeTable lexemes[MAX];
instruction codeOP[MAX];
int opIndex = 0, currindex = 0, level=0, table_size;
symbol_table symTable [MAX_SYM_TABLE_SIZE];



// Input helpers
int commentChecker(char code[], int i, int length);// Checks for comments
void processInput(char code[], int length, lexemeTable lexemes[], int *lexemeC, reservednames rNames[], specialSym sym[]);// Process input and generate lexemes

// Lexeme helper functions
void emit(int opNum,int l, int m);// Sets up operation
void errorMessage(int error);// Displays error message according to each
void printOperations();// Prints operations

// Symbol table
int symbolTableCheck(char *name, int level, int mark, int kind);// Linear search through symbol table looking at name
void symboltable_insert(int k, char *name, int val, int level, int addr, int mark);// Inserts to symbol table
void markLevel(int level); // Marks level to 1 as we can't access it

// Parser
void program();// Start and end of program
void block(int level, int tableIndex);// Look from beginning to end of a block
void proc_dec(int level, int *identNum);// Looks at procedure declarations
void const_dec(int level, int *identNum);// Deals with constant declarations
int var_dec(int level, int *identNum);// Deals with variable declaration
void statement(int level);// Deals with code statements
void condition(int level); // Conditional statements
void expression(int level);// Math expressions
void term(int level); // Deals with terms
void factor(int level); // Deals with factors

FILE *outfile;// elf.txt

// Main Method
int main(int argc, char *argv[]){
    if (argc != 2) {
        printf("Usage: %s <input_filename>\n", argv[0]);
        return 1;
   }

    char ch, code[MAX];
    int lexemeCfr = 0;
    int *lexemeC = &lexemeCfr, length = 0;

    FILE *infile = fopen(argv[1], "r");
    outfile = fopen("elf.txt", "w");

    if (infile == NULL ){
        printf("File Could not be opened\n");
        exit(0);
    }

    rewind(infile);

    while (fscanf(infile, "%c", &ch) != EOF){
        code[length++] = ch;
        printf("%c", ch);
    }

    processInput(code, length, lexemes, lexemeC, rNames, sym);
    program();

    printf("\nNo errors, program is syntactically correct\n\n");
    printOperations(outfile);

    fclose(infile);

    return 0;
}

int commentChecker(char code[], int i, int length){
    if (code[i] == '/' && ((code[i+1]=='/'||code[i+1]=='*'))) //single line comment
    {
        int check;
        if (code[i+1]=='/')
        {
            check = i+1;
            while(code[check]!='\n')
            {
                check++;
            }
        }
        else//double line comment
        {
                check=i+1;
                bool k = true;
                while(k)
                {
                  if(code[check] == '*' && code[check + 1] == '/')
                  {
                    check += 2;
                    break;
                  }
                  check++;
                }
        }
         return check;
    }
    else
    {
        return 0;
    }
}

void processInput(char code[], int length, lexemeTable lexemes[], int *lexemeC, reservednames rNames[], specialSym sym[] ){
    int index = 0;
    char digit[MAX] = "";
    char currentname[MAX] = "";
    int cIndex;

    do {
        // Checks and ignores whitespace
        if (isspace(code[index]) || iscntrl(code[index])){
            index++;
            continue;
        }
        else if (ispunct(code[index])){
            cIndex = commentChecker(code, index, length); // ignoring comments

            if (cIndex != 0){
                index = cIndex + 1;
                continue;
            }

            int symFound = 0;

            // check if the sym is in the table
            for (int i = 0; i < NOR_SYM; i++){
                if (code[index] == sym[i].sym || code[index] == ':' || code[index] == '!'){
                    symFound = 1;
                    lexemes[*lexemeC].lexeme[0] = code[index];

                    if ((index + 1) < length && ispunct(code[index + 1])){
                        switch (code[index]){
                        case '<':
                            if (code[index + 1] == '>'){
                                lexemes[*lexemeC].tokenType = neqsym;
                                lexemes[*lexemeC].lexeme[1] = code[index + 1];
                                index++;
                            }
                            else if (code[index + 1] == '='){
                                lexemes[*lexemeC].tokenType = leqsym;
                                lexemes[*lexemeC].lexeme[1] = code[index + 1];
                                index++;
                            }
                            else{
                                lexemes[*lexemeC].tokenType = sym[i].token;
                            }
                            break;
                        case '>':
                            if (code[index + 1] == '='){
                                lexemes[*lexemeC].tokenType = geqsym;
                                lexemes[*lexemeC].lexeme[1] = code[index + 1];
                                index++;
                            }
                            else{
                                lexemes[*lexemeC].tokenType = sym[i].token;
                            }
                            break;
                        case ':':
                            if (code[index + 1] == '='){
                                lexemes[*lexemeC].tokenType = becomessym;
                                lexemes[*lexemeC].lexeme[1] = code[index + 1];
                                index++;
                            }
                            else{
                                lexemes[*lexemeC].lexeme[1] = code[index + 1];
                                index++;
                                errorMessage(-1); //error
                                continue;
                            }
                            break;
                        default:
                                lexemes[*lexemeC].lexeme[1] = '\0';
                                lexemes[*lexemeC].tokenType = -1;
                                errorMessage(-1); //error
                                index++;
                                (*lexemeC)++;
                                symFound = 0;
                                continue;
                            break;
                        }
                    }
                    else{
                        lexemes[*lexemeC].tokenType = sym[i].token;
                    }
                    index++;
                    break;
                }
            }
           // Check if the symbol is not found or if its tokenType is -1
            if (!symFound || lexemes[*lexemeC].tokenType == -80000000){
                lexemes[*lexemeC].lexeme[0] = code[index];
                lexemes[*lexemeC].lexeme[1] = '\0';
                errorMessage(-1); //error
                index++;
                (*lexemeC)++;
                continue;
            }

            (*lexemeC)++;
        }
        else if (isalpha(code[index])){
            int Lcount = 0;
            while (isalnum(code[index])){
                currentname[Lcount] = code[index];
                index++;
                Lcount++;
            }

            currentname[Lcount] = '\0';

            if (Lcount > CMAX){
                strcpy(lexemes[*lexemeC].lexeme, currentname);

                for (int i = 0; i < NO_RW; i++){
                    if (strcmp(rNames[i].name, currentname) == 0){
                        lexemes[*lexemeC].tokenType = rNames[i].token;
                        break;
                    }
                    else
                        lexemes[*lexemeC].tokenType = -1;
                }
                lexemes[*lexemeC].lexeme[strlen(currentname)] = '\0';
                (*lexemeC)++;
                errorMessage(-2); //error
                continue;
            }
            else{
                strcpy(lexemes[*lexemeC].lexeme, currentname);

                for (int i = 0; i < NO_RW; i++) {
                    if (strcmp(rNames[i].name, currentname) == 0){
                        lexemes[*lexemeC].tokenType = rNames[i].token;
                        break;
                    }
                    else
                        lexemes[*lexemeC].tokenType = identsym;
                }
                (*lexemeC)++;
            }
        }
        // char is a number
        else if (isdigit(code[index])){
            int digitC = 0;

            while (isdigit(code[index])){
                digit[digitC] = code[index];
                index++;
                digitC++;
            }

            digit[digitC++] = '\0';
            if (digitC <= DMAX){
                strcpy(lexemes[*lexemeC].lexeme, digit);
                lexemes[*lexemeC].numlexeme = atoi(digit);
                lexemes[*lexemeC].tokenType = numbersym;
                (*lexemeC)++;
            }
            else{
                strcpy(lexemes[*lexemeC].lexeme, digit);
                lexemes[*lexemeC].numlexeme =atoi(digit);
                lexemes[*lexemeC].tokenType = -3;
                (*lexemeC)++;
                errorMessage(-3); //error
                continue;
            }
        }
   } while (index < length);
}

void emit(int opNum,int l, int m){
    codeOP[opIndex].operatorNum = opNum;
    codeOP[opIndex].l = l;
    codeOP[opIndex].m = m;
    opIndex++;
}

void errorMessage(int error){
    printf("*****");

    switch(error) {
        case -1:
            printf("Error : invalid symbol\n");
            break;
        case -2:
            printf("Error : string over 11 characters\n");
            break;
        case -3:
            printf("Error : number over 5 digits\n");
            break;
        case 1:
            printf("Error : program must end with period\n");
            break;
        case 2:
            printf("Error : const, var, read, procedure keywords must be followed by identifier\n");
            break;
        case 3:
            printf("Error : symbol name has already been declared\n");
            break;
        case 4:
            printf("Error : constants must be assigned with =\n");
            break;
        case 5:
            printf("Error : constants must be assigned with integer value\n");
            break;
        case 6:
            printf("Error : undeclared identifier %s\n",lexemes[currindex].lexeme);
            break;
        case 7:
            printf("Error : only variable values may be altered\n");
            break;
        case 8:
            printf("Error : assignment statements must use :=\n");
            break;
        case 9:
            printf("Error : begin must be followed by end\n");
            break;
        case 10:
            printf("Error : if must be followed by then\n");
            break;
        case 11:
            printf("Error : while must be followed by do\n");
            break;
        case 12:
            printf("Error : condition must contain comparison operator\n");
            break;
        case 13:
            printf("Error : right parenthesis must follow left parenthesis\n");
            break;
        case 14:
            printf("Error : arithmetic equations must contain operands, parentheses, numbers, or symbols\n");
            break;
        case 15:
            printf("Error : constant and variable declarations must be followed by a semicolon\n");
        	  break;
        case 16:
            printf("Error : semicolon or end expected\n");
        	  break;
        case 17:
            printf("Error : passed levels limit\n");
        	  break;
        case 18:
            printf("Error : call must be followed by an identifier\n");
        	  break;
        case 19:
            printf("Error : only procedures can follow the keyword call\n");
        	  break;
        default:
            printf("Error : code two long (max code is 10000)");
            break;
    }
    exit(0);
}

int symbolTableCheck(char *name, int level, int mark, int kind){
    for(int i = table_size; i >= 0; i--){
        if (strcmp(symTable[i].name, name) == 0  && symTable[i].mark == 0){
            if(kind != -1){
                if(symTable[i].kind != kind)
                    continue;
            }
            if(symTable[i].level == level){
                return i;
            }
            else{
                return -i-1;
            }
        }
    }
    return -80000000;
}

void symboltable_insert(int k, char *name, int val, int level, int addr, int mark){
    if (table_size >= MAX_SYM_TABLE_SIZE){
        errorMessage(0);
    }

    if (symbolTableCheck(name, level, 0, k) > -1)
        errorMessage(3);//symbol name alr declared

    symTable[table_size].kind = k;
    strcpy(symTable[table_size].name, name);
    symTable[table_size].level = level;
    symTable[table_size].val = val;
    symTable[table_size].addr = addr;
    symTable[table_size].mark = mark;
    table_size++;
}

void program(){
    int tableIndex = 0;

    block(level, tableIndex);

    if (lexemes[currindex].tokenType != periodsym){ //periodsym
        errorMessage(1);
    }

    emit(9,0,3);//halt
}

void block(int level, int tableIndex){
    int dx = 3, tx0 = tableIndex, jump = opIndex;
    emit(7,0,0);//jump


    if(level > 5)
        errorMessage(17);//too many level

    do {
        do { const_dec(level, &tableIndex); } while( lexemes[currindex].tokenType == identsym);
        do { dx += var_dec(level, &tableIndex);   } while( lexemes[currindex].tokenType == identsym);
        while(lexemes[currindex].tokenType == procsym){ proc_dec(level, &tableIndex);}

    }while((lexemes[currindex].tokenType == constsym)|| (lexemes[currindex].tokenType == varsym)|| (lexemes[currindex].tokenType == procsym));

    codeOP[jump].m = opIndex*3;
    emit(6,0,dx);
    statement(level);

    if( level != 0)
      emit(2,0,0);
}

void proc_dec(int level, int *identNum){
    if(lexemes[currindex].tokenType == procsym){
        currindex++;
        *identNum = 1;
        do {
            if(lexemes[currindex].tokenType != identsym)
                errorMessage(2);

            int k = symbolTableCheck(lexemes[currindex].lexeme, level, 0, 3);

            if(k > -1)
                errorMessage(3);//already been declared


            symboltable_insert(3, lexemes[currindex].lexeme,lexemes[currindex].numlexeme,level,opIndex*3,0);
            currindex++;

            if(lexemes[currindex].tokenType != semicolonsym)
                errorMessage(15);

            currindex++;

            block(level+1, *identNum-1);

            if(lexemes[currindex].tokenType != semicolonsym)
                errorMessage(16);

            currindex++;

        }while(lexemes[currindex].tokenType == semicolonsym);

        markLevel(level+1);
    }
}

void markLevel(int level){
    for(int i = table_size; i >= 0; i--){
        if(symTable[i].level == level){
            symTable[i].mark = 1;
        }
    }
}

void const_dec(int level, int *identNum){
    char token_name[CMAX];
    if (lexemes[currindex].tokenType == constsym) { //start of a const declaration
        do {
            currindex++;

            if (lexemes[currindex].tokenType != identsym)
                errorMessage(2); //expecting an identifier after const, var, and read

            if(symbolTableCheck(lexemes[currindex].lexeme, level, 0, 1) > -1){
                errorMessage(3);//already been declared
              }

            strcpy(token_name, lexemes[currindex].lexeme);
            currindex++;
            if (lexemes[currindex].tokenType != eqlsym)
                errorMessage(4); //must be assigned with =

            currindex++;
            if (lexemes[currindex].tokenType != numbersym)
                errorMessage(5); //must be assigned an integer value

            symboltable_insert(1, token_name,lexemes[currindex].numlexeme,level,*identNum + 2,0);
            (*identNum)++;

            currindex++;

        } while (lexemes[currindex].tokenType == commasym); // Continue if there's a comma

        if (lexemes[currindex].tokenType != semicolonsym)
            errorMessage(15); //must be followed by a semicolon

        currindex++;

        const_dec(level, identNum);
    }
}

int var_dec(int level, int *identNum){
    int numVars = 0;
    if (lexemes[currindex].tokenType == varsym){
        bool in = false;
        do {
            numVars++;
            currindex++;
            (*identNum)++;

            if (lexemes[currindex].tokenType != identsym){//identsym
                int t = currindex;
                for (int i = 0; i < NO_RW; i++){
                    if(lexemes[t].tokenType == semicolonsym){
                        currindex = t;
                        break;
                    }
                    if(lexemes[t].tokenType == commasym){
                        t++;
                    }
                    if (lexemes[t].tokenType == rNames[i].token){
                        lexemes[t].tokenType = identsym;

                        if(symbolTableCheck(lexemes[t].lexeme,level,0, 2) > -1)
                            errorMessage(3);//already been declared

                        symboltable_insert(2, lexemes[t].lexeme, 0, level, *identNum + 2,0);

                        t++; currindex++; i=0; numVars++;
                        in = true;
                    }
                }
                if(!in)
                    errorMessage(2);//expected identifier
            }
           if(in)
                break;

           if(symbolTableCheck(lexemes[currindex].lexeme,level,0, 2) > -1)
                errorMessage(3);//already been declared

            symboltable_insert(2, lexemes[currindex].lexeme, 0, level, *identNum + 2,0);
            currindex++;

        } while (lexemes[currindex].tokenType == commasym); //commasym

        if (lexemes[currindex].tokenType != semicolonsym) //semicolomsym
           errorMessage(15);//need semicolon

        currindex++;
        var_dec(level, identNum);
    }
    return numVars;
}

void statement(int level){
    if (lexemes[currindex].tokenType == identsym){//IDENTSYM
        int symIndex = symbolTableCheck(lexemes[currindex].lexeme, level, 0, -1);

        if (symIndex == -80000000)
            errorMessage(6);//undeclared identifier

        if(symIndex < 0)
            symIndex = (symIndex * -1)-1;

        if (symTable[symIndex].kind != 2){//not a var
            symIndex = symbolTableCheck(lexemes[currindex].lexeme, level, 0, 2);

            if(symIndex == -80000000){
                errorMessage(7);//only variable values may be altered
            }
            else if(symIndex < 0){
                symIndex = (symIndex * -1)-1;
            }
        }
        currindex++;

        if (lexemes[currindex].tokenType != becomessym) //becomessym
            errorMessage(8);//assignments must use :=

        currindex++;
        expression(level);
        emit(4,level-symTable[symIndex].level, symTable[symIndex].addr);//STO
    }
    else if(lexemes[currindex].tokenType == callsym){
        currindex++;
        if(lexemes[currindex].tokenType != identsym)
            errorMessage(18);

        int symIndex = symbolTableCheck(lexemes[currindex].lexeme, level, 0, -1);

        if (symIndex == -80000000)
            errorMessage(6);//undeclared identifier

        if(symIndex < 0)
            symIndex = (symIndex * -1)-1;

        if (symTable[symIndex].kind != 3){
            symIndex = symbolTableCheck(lexemes[currindex].lexeme, level, 0, 3);

            if(symIndex == -80000000){
                errorMessage(19);//not a procedure
            }
            else if(symIndex < 0){
                symIndex = (symIndex * -1)-1;
            }
        }
        emit(5,level-symTable[symIndex].level,symTable[symIndex].addr);//call
        currindex++;
    }
    else if (lexemes[currindex].tokenType == beginsym){//BEGINSYM
        do {
            currindex++;
            statement(level);
        } while (lexemes[currindex].tokenType == semicolonsym);

        if(lexemes[currindex].tokenType != endsym)
            errorMessage(9);//begin needs an end

        currindex++;
    }
    else if (lexemes[currindex].tokenType == ifsym){//IFSYM
        currindex++;
        condition(level);
        int jpcIdx = opIndex;//currindex;
        emit(8,0,opIndex);//emit jpc
        if (lexemes[currindex].tokenType != thensym)
            errorMessage(10);//if needs a then;

        currindex++;
        statement(level);
        codeOP[jpcIdx].m = opIndex*3;
    }
    else if(lexemes[currindex].tokenType == whilesym){//WHILESYM
        currindex++;
        int loopIdx = currindex;
        condition(level);
        if(lexemes[currindex].tokenType != dosym)
            errorMessage(11);//whiles needs a do
        currindex++;
        int jpcIdx = opIndex;
        emit(8,0,opIndex);//emit jpc
        statement(level);
        emit(7,0,loopIdx);
        codeOP[jpcIdx].m = opIndex*3;
    }
    else if(lexemes[currindex].tokenType == readsym){
        currindex++;
        if(lexemes[currindex].tokenType != identsym)
            errorMessage(2);

        int symIndx = symbolTableCheck(lexemes[currindex].lexeme, level, 0, -1);

        if (symIndx == -80000000)
            errorMessage(6);//undeclared identifier

        if(symIndx < 0)
            symIndx = (symIndx * -1)-1;

        if (symTable[symIndx].kind != 2){//not a var
            symIndx = symbolTableCheck(lexemes[currindex].lexeme, level, 0, 2);

            if(symIndx == 80000000){
                errorMessage(7);//needs to be var
            }
            else if(symIndx < 0){
                symIndx = (symIndx * -1)-1;
            }
        }
        currindex++;
        emit(9,0,2);//read
        emit(4,level-symTable[symIndx].level,symTable[symIndx].addr);//STO
    }
    else if(lexemes[currindex].tokenType == writesym){//writesym
        currindex++;
        expression(level);
        emit(9,0,1);
    }
}

void condition(int level){
    if(lexemes[currindex].tokenType == oddsym){
        currindex++;
        expression(level);
        emit(2,0,11);//ODD
   }
   else{
        expression(level);
        if(lexemes[currindex].tokenType == eqlsym){
            currindex++;
            expression(level);
            emit(2,0,5);//eql
        }
        else if(lexemes[currindex].tokenType == neqsym){
            currindex++;
            expression(level);
            emit(2,0,6);//neq
        }
        else if(lexemes[currindex].tokenType == lessym){
            currindex++;
            expression(level);
            emit(2,0,7);//leq
        }
        else if(lexemes[currindex].tokenType == gtrsym){
            currindex++;
            expression(level);
            emit(2,0,9);//gtr
        }
        else if(lexemes[currindex].tokenType == geqsym){
            currindex++;
            expression(level);
            emit(2,0,10);//geq
        }
        else
            errorMessage(12);//condition must have comparison op
   }
}

void expression(int level){
    if(lexemes[currindex].tokenType == minussym){
        while(lexemes[currindex].tokenType == plussym || lexemes[currindex].tokenType == minussym){
            if(lexemes[currindex].tokenType == plussym){
                currindex++;
                term(level);
                emit(2,0,1);//add
            }
            else {
                currindex++;
                term(level);
                emit(2,0,2);//sub
            }
        }
    }
    else{
        if(lexemes[currindex].tokenType == plussym)
            currindex++;
        term(level);
        while(lexemes[currindex].tokenType == plussym || lexemes[currindex].tokenType == minussym){
            if(lexemes[currindex].tokenType == plussym){
                currindex++;
                term(level);
                emit(2,0,1);//add
            }
            else {
                currindex++;
                term(level);
                emit(2,0,2);//sub
            }
        }
    }
}

void term(int level){
    factor(level);
    while(lexemes[currindex].tokenType == multsym || lexemes[currindex].tokenType == slashsym){
       if(lexemes[currindex].tokenType == multsym){
            currindex++;
            factor(level);
            emit(2,0,3);//mul
       }
       else if(lexemes[currindex].tokenType == slashsym){
            currindex++;
            factor(level);
            emit(2,0,4);//div
       }
     }
}

void factor(int level){
    if(lexemes[currindex].tokenType == identsym){
        int symIdx = symbolTableCheck(lexemes[currindex].lexeme,level,0,-1);

        if (symIdx == -80000000)
            errorMessage(6);//undeclared variable

        if(symIdx < 0)
            symIdx = (symIdx * -1)-1;

        if(symTable[symIdx].kind == 1){
            emit(1,0,symTable[symIdx].val);//lit
        }
        else{
            emit(3,level-symTable[symIdx].level,symTable[symIdx].addr);//lod
        }
        currindex++;
    }
    else if(lexemes[currindex].tokenType == numbersym){
        emit(1,0,lexemes[currindex].numlexeme);//lit
        currindex++;
    }
    else if(lexemes[currindex].tokenType == lparentsym){
        currindex++;
        expression(level);
        if(lexemes[currindex].tokenType != rparentsym)
            errorMessage(13);
        currindex++;
    }
    else
        errorMessage(14);//factor needs to be op, pare, num, or sym
}

void printOperations(FILE *outfile) {
    printf("Assembly Code:\n");
    printf("OP  L   M\n");

    for(int i = 0; i < opIndex; i++){
        fprintf(outfile, "%d %d %d\n", codeOP[i].operatorNum, codeOP[i].l, codeOP[i].m);
        printf("%d   %d   %d\n", codeOP[i].operatorNum, codeOP[i].l, codeOP[i].m);
    }
}
