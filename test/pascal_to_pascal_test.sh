c=0
for i in `ls exemple/*` 
do
a=`perl -p -e 's/\{.+?\}//gs' exemple/arith2.p | tr -d ' \t\n\r\f'`
b=`cat exemple/arith2.p | bin/pascal_to_pascal | tr -d ' \t\n\r\f'`
if [ $a != $b ]
then
echo probleme avec $i
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