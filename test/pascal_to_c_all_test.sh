c=0
for i in `ls unit_test/*` 
do
	a=$(test/pascal_to_c_test.sh $i)
	if [[ $? -eq 1 ]]
	then
		echo problem with $i :
		echo "$a"
		c=$(($c+1))
	fi
done
if [[ $c -eq 0 ]]
then
	echo no problem : all unit test pass
	exit 0
fi
echo $c problems
exit 1