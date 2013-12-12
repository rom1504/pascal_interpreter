void writeln(int a)
{
	printf("%d ",a);
}

int main()
{
	int i,j,k,l,m;
	i=30;
   j = (((((i*i)+(i+i)+(i*i)+(i+i))+((i*i)+(i+i)+(i*i)+(i+i)))+(((i*i)+(i+i)+(i*i)+(i+i))+((i*i)+(i+i)+(i*i)+(i+i))))+((((i*i)+(i+i)+(i*i)+(i+i))+((i*i)+(i+i)+(i*i)+(i+i)))+(((i*i)+(i+i)+(i*i)+(i+i))+((i*i)+(i+i)+(i*i)+(i+i)))));
   writeln(j);
   k = j+i;
   writeln(k);
   l = j+k+i;
   writeln(l);
   m = l+j+k+i;
   writeln(m);
   writeln((((i-i)-i)-i)-i);
   writeln(i-(i-(i-(i-i))));
   writeln(i * -10);
   writeln(i / (4));
   writeln(i - (i / 4 )*4);
}
