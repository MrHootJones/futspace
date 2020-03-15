run: 
		futhark opencl futspace.fut
		cat ../data/binary-fut-arrays/C1W.in ../data/binary-fut-arrays/D1.in | ./futspace -b -D > img_map.data
		python ../tools/data2png.py img_map.data success.png
		rm img_map.data