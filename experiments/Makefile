deqbin = ../.stack-work/install/x86_64-linux/lts-9.14/8.0.2/bin
nlambdabin = nlambda/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/bin
loisbin = lois/stacks

lois-allpairs:
	num1=1 ; while [[ $$num1 -le 14 ]] ; do \
		num2=1 ; while [[ $$num2 -le 14 ]] ; do \
			$(loisbin)/nlambda-exe --lrsz=$$num1 --rlsz=$$num2 >> lois-allpairs.txt ; \
			((num2 = num2 + 1)) ; \
		done ; \
		((num1 = num1 + 1)) ; \
	done

nlambda-allpairs:
	num1=1 ; while [[ $$num1 -le 14 ]] ; do \
		num2=1 ; while [[ $$num2 -le 14 ]] ; do \
			$(nlambdabin)/nlambda-exe --lrsz=$$num1 --rlsz=$$num2 >> nlambda-allpairs.txt ; \
			((num2 = num2 + 1)) ; \
		done ; \
		((num1 = num1 + 1)) ; \
	done

deq-allpairs:
	num1=1 ; while [[ $$num1 -le 14 ]] ; do \
		num2=1 ; while [[ $$num2 -le 14 ]] ; do \
			$(deqbin)/stacks-exe --lrsz=$$num1 --rlsz=$$num2 >> deq-allpairs.txt ; \
			((num2 = num2 + 1)) ; \
		done ; \
		((num1 = num1 + 1)) ; \
	done

scale:
	num1=100 ; while [[ $$num1 -le 2400 ]] ; do \
		$(deqbin)/stacks-exe --lrsz=$$num1 --rlsz=$$num1 >> scale.txt ; \
		((num1 = num1 + 100)) ; \
	done

onestate:
	num1=200 ; while [[ $$num1 -le 500 ]] ; do \
		$(deqbin)/one-exe -n=$$num1 >> one.txt ; \
		((num1 = num1 + 10)) ; \
	done

glbl:
	num1=100 ; while [[ $$num1 -le 2500 ]] ; do \
		$(deqbin)/global-exe -n=$$num1 >> global.txt ; \
		((num1 = num1 + 100)) ; \
	done
