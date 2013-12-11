if [[ $# -ne 1 ]]
then
		echo "usage : $0 <test file>" 
		exit 1
fi

test_file="$1"
name=`basename $test_file`
name=${name%.*}
pascal_file="exemple/$name.p"
i=0
while read -r line
do
	lines[$i]=$line
	i=$(($i+1))
done < "$test_file"

c=0
for line in "${lines[@]}"
do
	input=`echo $line | cut -f1 -d':'`
	expected_output=`echo $line  | cut -f2 -d':'`
	echo input : $input
 	echo expected output : $expected_output
 	actual_output=$(echo $input | tr " " "\n" | bin/pascal_interpreter "$pascal_file" | tr "\n" " "  | sed 's/ *$//g')
 	echo actual output : $actual_output
 	if [ "$expected_output" = "$actual_output" ]
 	then
		echo ok
		
 	else
		echo not ok
		c=$(($c+1))
 	fi
done

if [[ $c -eq 0 ]]
then
echo no problem : all units tests pass
exit 0
fi
echo $c problems
exit 1