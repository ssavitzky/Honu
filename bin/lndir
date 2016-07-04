#!/bin/sh

DIRFROM=$1
DIRTO=$2

USAGE="Usage: $0 fromdir todir"

case "$DIRFROM" in
/*)	;;
*)	echo "dir \"$DIRFROM\" must begin with a /"
	exit 1
	;;
esac

if [ ! -d $DIRFROM -a ! -d $DIRTO ]
then
	echo "$USAGE"
	exit 1
fi

REALFROM=

DIRLIST=`(cd $DIRFROM; find * \( -type d ! -name 'RCS' \) -print)`

cd $DIRTO

if [ `(cd $DIRFROM; pwd)` = `pwd` ]
then
	echo "FROM and TO are identical!"
	exit 1
fi

for dir in $DIRLIST
do
	mkdir $dir
done

for file in `ls $DIRFROM`
do
	ln -s $DIRFROM/$file .
done

for dir in $DIRLIST
do
	echo $dir:
	(cd $dir;
	 if [ `(cd $DIRFROM/$dir; pwd)` = `pwd` ]
	 then
		echo "FROM and TO are identical!"
		exit 1
	 fi;
	 for file in `ls $DIRFROM/$dir`
	 do
		ln -s $DIRFROM/$dir/$file .
	 done)
done
