void writeln(int a)
{
	printf("%d ",a);
}

int tak (int x, int y, int z)
{
   if (y < x) return 1 + tak (tak (x-1, y, z), tak (y-1, z, x), tak(z-1, x, y));
   else return z;
}

void main()
{
   writeln(tak(18, 12, 6));
}
