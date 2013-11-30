export OCAMLRUNPARAM='p'
for i in `ls exemple/*` 
do
echo $i
cat $i | bin/pascal_to_pascal 2>&1| grep error
done
exit 0