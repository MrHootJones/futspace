run: 
		futhark opencl futspace.fut
		cat program-inputs/C1W.in program-inputs/D1.in | ./futspace -b -D > img_map.data
		python data2png.py img_map.data success.png
		rm img_map.data
run3: 
		futhark opencl futspace.fut
		cat program-inputs/C18W.in program-inputs/D18.in | ./futspace -b -D > img_map.data
		python3 data2png.py img_map.data success.png
		rm img_map.data
