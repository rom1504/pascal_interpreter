void f (int x)
{
   if (x == 0) printf("0 ");
   else if (x == 1)  printf("1 ");
   else f (x - 2);
}

void g (int x)
{
   if (x == 0) printf("0 ");
   else if (x == 1 )  printf("1 ");
   else
   {
      g (x - 2);
      g (x - 1);
   }
}

void main()
{
   f(10);
   g(10);
}

