c=0
for i in `ls exemple/*` 
do
a=`cat $i | tr -d ' \t\n\r\f' | perl -p -e 's/\{.+?\}//g'`
b=`cat $i | bin/pascal_to_pascal | tr -d ' \t\n\r\f'`
if [ $a != $b ]
then
echo probleme avec $i
echo expected : $a
echo actual : $b
c=$(($c+1))
fi
done
if [[ $c -eq 0 ]]
then
echo aucun probleme : pascal_to_pascal renvoie bien le même code sur les exemples
exit 0
fi
echo $c problemes
exit 1