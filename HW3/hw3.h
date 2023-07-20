#ifndef __HW3_H
#define __HW3_H


typedef enum { NUM, STRING, OTHER } ExprType;

typedef struct NumNode {
	double numValue;
	int trail;
} NumNode;

typedef struct ExprNode {
	ExprType exprType;
	char * stringValue;
	NumNode numNode;
	int pr;
	int line;
} ExprNode;

typedef struct Info {
	ExprNode exprNode;
	int line;
	struct Info *next;
	int mismatch;
} Info;

ExprNode mkAdd(ExprNode a , ExprNode b, int line );
ExprNode mkSub(ExprNode a, ExprNode b, int line);
ExprNode mkMul(ExprNode a, ExprNode b, int line);
ExprNode mkDiv(ExprNode a, ExprNode b, int line);
ExprNode mkStr(char * str, int pr, int line);
ExprNode mkNum(NumNode a, int pr, int line);
ExprNode mkOther();
char *concat(const char *str1, const char *str2);
double rounding(double a);
void info_adder(struct Info *head, ExprNode a,int m);
void printer(struct Info *head);
Info *initial();
#endif

