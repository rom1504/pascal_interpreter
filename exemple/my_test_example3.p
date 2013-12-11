program

var x : integer;  arr : array of integer;

procedure arraydouble (arr : array of integer; siz : integer);
begin
	 arr[0]:=arr[0];
end;

begin
   arr := new array of integer [1];
   arr[0]:=1;
   arraydouble (arr,1);

end.