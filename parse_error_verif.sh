export OCAMLRUNPARAM='p'
c=0
for i in `ls exemple/*` 
do
cat $i | bin/pascal_to_pascal 2>&1| grep -q error
if [ $? -eq 0 ]
then
echo probleme avec $i
echo $a
c=1
fi
done
if [[ $c -eq 0 ]]
then
echo aucun probleme : aucune erreur de parse sur les exemples
exit 0
fi
exit 1