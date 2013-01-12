#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define STACKSIZE (1024 * 64)
#define MAX_SRCSIZE 30000
#define MAX_CODESIZE 30000
//#define DEBUG printf
#define DEBUG(...) 

enum {
	// basic instruction
	INS_PUSH,
	INS_POP,
	INS_ADD,
	INS_SUB,
	INS_JMP,
	INS_INPUT,
	INS_OUTPUT,
	// for optimize
	INS_PUSH_N,
	INS_POP_N,
	INS_ADD_N,
	INS_SUB_N,
	// original
	INS_EXIT,
};

static void *jmptbl[256] = {0};

static void compile(char *src, int size, void **code) {
	int stack[64];
	int si = 0;
	int ci = 0;
	for(int i=0; i<size; i++) {
		DEBUG("%c", src[i]);
		switch(src[i]) {
		case '>':
			code[ci++] = jmptbl[INS_PUSH];
			break;
		case '<':
			code[ci++] = jmptbl[INS_POP];
			break;
		case '+':
			code[ci++] = jmptbl[INS_ADD];
			break;
		case '-':
			code[ci++] = jmptbl[INS_SUB];
			break;
		case '.':
			code[ci++] = jmptbl[INS_OUTPUT];
			break;
		case ',':
			code[ci++] = jmptbl[INS_INPUT];
			break;
		case '[':
			stack[si++] = ci;
			ci+=3;
			//DEBUG("jump begin(nest=%d pos=%d codeindex=%d)\n", si, i, ci);
			break;
		case ']':
			//DEBUG("jump close(nest=%d pos=%d codeindex=%d)\n", si, i, ci);
			assert(si > 0);
			si--;
			// [
			code[stack[si]  ] = jmptbl[INS_JMP];
			code[stack[si]+1] = code + stack[si] + 3;	// then
			code[stack[si]+2] = code + ci + 3;				// else
			// ]
			code[ci  ] = jmptbl[INS_JMP];
			code[ci+1] = code + stack[si] + 3;	// then
			code[ci+2] = code + ci + 3;					// else
			ci += 3;
			break;
		}
	}
	code[ci++] = jmptbl[INS_EXIT];
	DEBUG("generate %d codes\n", ci);
}

static void run(void **pc_)
{
	if(pc_ == NULL){
		jmptbl[INS_ADD] = &&L_ADD;
		jmptbl[INS_SUB] = &&L_SUB;
		jmptbl[INS_PUSH] = &&L_PUSH;
		jmptbl[INS_POP] = &&L_POP;
		jmptbl[INS_JMP] = &&L_JMP;
		jmptbl[INS_INPUT] = &&L_INPUT;
		jmptbl[INS_OUTPUT] = &&L_OUTPUT;
		jmptbl[INS_EXIT] = &&L_EXIT;
		return;
	}
	static int stack[STACKSIZE];
	memset(stack, 0, STACKSIZE);
	register int *sp = stack;
	register void **pc = pc_;
	goto **pc;

L_PUSH:// >
	sp++;
	goto **(++pc);

L_POP: // <
	sp--;
	goto **(++pc);

L_ADD: // +
	(*sp)++;
	goto **(++pc);

L_SUB: // -
	(*sp)--;
	goto **(++pc);

L_JMP: // [, ]
	goto **(pc = (void **)(*sp ? pc[1] : pc[2]));

L_OUTPUT:// .
	putchar(*sp);
	goto **(++pc);

L_INPUT: // ,
	*sp = getchar();
	goto **(++pc);

L_EXIT:
	return;
}

//void interactive(){
//	while(1){
//		char in[256];
//		printf(">>>");
//		fgets(in, 256, stdin);
//		void *code[1024];
//		compile(code, in);
//		exec(code);
//		putchar('\n');
//	}
//}

static void runFromFile(const char *fname) {
	FILE *fp = fopen(fname, "r");
	if(fp == NULL) {
		fprintf(stderr, "File open error: %s\n", fname);
		exit(1);
	}
	char *src = (char *)malloc(MAX_SRCSIZE);
	int size = fread(src, 1, MAX_SRCSIZE, fp);
	fclose(fp);
	DEBUG("open %s, read %dbyte\n", fname, size);

	void **code = (void **)malloc(sizeof(void *) * MAX_CODESIZE);
	compile(src, size, code);
	run(code);

	free(src);
	free(code);
}

int main(int argc, char **argv) {
	run(NULL);
	for(int i=1; i<argc; i++){
		runFromFile(argv[i]);
		return 0;
	}
	//interactive();
	return 0;
}

