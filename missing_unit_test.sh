echo No unit test for :
c=0
for example_file in `ls exemple/*`
do
	name=`basename $example_file`
	name=${name%.*}
	test_file="unit_test/$name.test"
	if [[ ! (-e $test_file) ]]
	then
		echo $test_file
		c=$(($c+1))
	fi
done
echo $c example files without unit test