#!/bin/bash

LOWERTWBND=14
UPPERTWBND=15
ENC=steiner.lp
rm -rf ground
rm -rf unsat
rm -rf unsat
rm -rf nontw
rm -rf nontwlow
mkdir unsat
mkdir nontw
mkdir nontwlow
mkdir unsat
mkdir ground
cd nonground
for i in *.lp
do
	echo $i
	gringo -t ../$ENC $i | grep -v '^edge\|^weight\|^eweight\|^vertex' > ../tmpf
	gringo ../tmpf | sed "s/,-/,/g" > ../ground/$i.$ENC.grounded
	#gringo $i ../steiner.lp ../*.req ../*.add > ../ground/$i.grounded
	#gringo $i ../monolithic.lp ../*.req ../*.add > ../ground/$i.mono.grounded
	#cat ../ground/$i.grounded | clasp-banane --opt-strategy=usc,stratify -n 1 
	tw=`cat ../ground/$i.$ENC.grounded | dynasp_old -d | grep "TREEWIDTH:\ " | sed 's/TREEWIDTH: //g'`
	echo $tw
	if [[ $tw -ge $LOWERTWBND ]] && [[ $tw -le $UPPERTWBND ]]; then
	cat ../ground/$i.$ENC.grounded | clasp-banane --opt-strategy=usc,13 --opt-usc-trim=min -n 1
	if [ "$?" -eq "20" ] 
	then
		mv ../ground/$i.$ENC.grounded unsat
		#mv ../ground/$i.mono.grounded unsat
		bzip2 unsat/$i.$ENC.grounded
		#bzip2 unsat/$i.mono.grounded
	else
		bzip2 ../ground/$i.$ENC.grounded
		#bzip2 ../ground/$i.mono.grounded
	fi
	else
		if [[ $tw == "" ]] || [[ $tw -gt $UPPERTWBND ]]
		then	
			mv ../ground/$i.$ENC.grounded ../nontw
			#mv ../ground/$i.mono.grounded unsat
			bzip2 ../nontw/$i.$ENC.grounded
		else
			mv ../ground/$i.$ENC.grounded ../nontwlow
			#mv ../ground/$i.mono.grounded unsat
			bzip2 ../nontwlow/$i.$ENC.grounded

		fi
	fi
	#gringo $i ../monolithic.lp ../*.req ../*.add > ../ground/$i.mono.grounded
	#cat ../ground/$i.mono.grounded | clasp -n 1 
	#if [ "$?" == "20" ] 
	#then
	#	mv ../ground/$i.mono.grounded ../unsat
	#	bzip2 ../unsat/$i.mono.grounded
	#else
	#	bzip2 ../ground/$i.mono.grounded
	#fi
done
cd ..

