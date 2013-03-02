#include <stdio.h>

char buf[128];



void printError()
{
  printf("Program called error!");
  exit(-1);
}

int readInt()
{
  for(;;) {
    int v;
    fgets(buf,sizeof(buf),stdin);
    if(1 == sscanf(buf,"%d",&v))
      return v;
    else {
      if(strlen(buf) > 0 && buf[strlen(buf)-1] == '\n')
	buf[strlen(buf)-1] = 0;
      printf("\"%s\" is not an int, try again:",buf);
    }
  }
}

double readDouble(void)
{
  for(;;) {
    double v;
    fgets(buf,sizeof(buf),stdin);
    if(1 == sscanf(buf,"%lg",&v))
      return v;
    else {
      if(strlen(buf) > 0 && buf[strlen(buf)-1] == '\n')
	buf[strlen(buf)-1] = 0;
      printf("\"%s\" is not a double, try again:",buf);
    }
  }
}

void printInt(int v)
{
  printf("%d\n",v);
}

void printDouble(double v)
{
  printf("%f\n",v);
}

void printString(char* s)
{
  printf("%s\n",s);
}

int main () {
  printInt(7 * readInt ()) ;
  printDouble(3.14);
  printString("hello");
  }
