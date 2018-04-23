deqbin = ../.stack-work/install/x86_64-linux/lts-9.14/8.0.2/bin

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
