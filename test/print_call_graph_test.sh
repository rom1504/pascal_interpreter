c=0
for i in `ls exemple/*` 
do
name=`basename $i`
name=${name%.*}
name=resultats_attendus/$name.dot
if [ ! -f $name ]
then
continue
fi
a=`cat $i | bin/print_call_graph`
b=`cat $name`
if [ "$a" != "$b" ]
then
echo probleme avec $i
diff  <(echo "$a" ) <(echo "$b")
c=$(($c+1))
fi
done
if [[ $c -eq 0 ]]
then
echo aucun probleme : print_call_graph renvoie bien le graphe attendu sur les exemples
exit 0
fi
echo $c problemes
exit 1