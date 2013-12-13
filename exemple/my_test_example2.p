program
var  a : array of array of array of array of integer ; b : array of boolean; c : integer ; u : integer ; g : integer ;

function lala () : integer;
var i : integer;
begin
	g:=g+1;
	i:=i+1;
	if i=1 and g<5 then begin u:=lala(); writeln(456) end else writeln(i+56);
	c:=6;
	lala:=0
end;

begin
	a:=new array of array of array of array of integer[5];
	a[1]:=new array of array of array of integer[5];
	a[1][2]:=new array of array of integer[5];
	a[1][2][3]:=new array of integer[5];
	a[1][2][3][4] := 42;
	a[1][2][3][2] := 69;
	writeln(a[1][2][3][2]);
 	{b := new array of boolean[6];
 	b[5]:=1}
 	writeln( (7 - 1 - -(-8)));
 	c:=5;
 	writeln(c);
 	u:= lala();
 	writeln(c)
end.
