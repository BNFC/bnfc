int main () {
  printString("What string? ") ;
  string s = readString() ;
  printString("How many times? ") ;
  int k = readInt() ;
  printString(replicate(k,s)) ;
  return ;
}

string replicate(int k, string s) {
  int i = 1 ;
  string r = s ;
  while (i < k){
    r = s + r ;
    i++ ;
  }
  return r ;
}

