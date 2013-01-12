#include <stdio.h>
#include <stdlib.h>

void *g_table[256] = {0};

void exec(void **pc);

void compile(char *pc){
	void *g_code[1024];
	exec(NULL);
	int n = 0;
	void **code = g_code;
	while(*pc != '\0'){
		if(g_table[*pc] == NULL){
			pc++;
			continue;
		}
		*code++ = g_table[*pc];
		if(*pc == '['){
			char *p = pc;
			int c = 1;
			int a = 0;
			while(c != 0){
				p++;
				a++;
				if(*p == '[') { c++; a+=2; }
				if(*p == ']') { c--; a+=2; }
			}
			code[0] = code + 2;
			code[1] = code + a + 2;
			code += 2;
		}else if(*pc == ']'){
			char *p = pc;
			int c = 1;
			int a = 0;
			while(c != 0){
				p--;
				a++;
				if(*p == ']') { c++; a+=2; }
				if(*p == '[') { c--; a+=2; }
			}
			code[0] = code - a + 2;
			code[1] = code + 2;
			code += 2;
		}
		pc++;
	}
	*code++ = g_table['\0'];
	exec(g_code);
}

void exec(void **pc_)
{
	if(pc_ == NULL){
		g_table['\0'] = &&L_END;
		g_table['+'] = &&L_ADD;
		g_table['-'] = &&L_SUB;
		g_table['<'] = &&L_POP;
		g_table['>'] = &&L_PUSH;
		g_table['['] = &&L_JMP;
		g_table[']'] = &&L_JMP;
		g_table['.'] = &&L_OUTPUT;
		g_table[','] = &&L_INPUT;
		return;
	}
	static int g_stack[1024];
	memset(g_stack, 0, 1024);
	register int *sp = g_stack;
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
	goto **(pc = *sp ? pc[1] : pc[2]);

L_OUTPUT://.
	putchar(*sp);
	goto **(++pc);

L_INPUT: // ,
	*sp = getchar();
	goto **(++pc);

L_END:
	return;
}

void interactive(){
	while(1){
		char in[256];
		printf(">>>");
		fgets(in, 256, stdin);
		compile(in);
		putchar('\n');
	}
}

int main(int argc, char **argv)
{
	int i;
	for(i=1; i<argc; i++){
		FILE *fp = fopen(argv[i], "r");
		char *data = (char *)malloc(30000);
		fread(data, 30000, 1, fp);
		fclose(fp);
		compile(data);
		putchar('\n');
		break;
	}
	interactive();
	return 0;
}

