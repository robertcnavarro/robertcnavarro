for infile in *.fastq.gz
do
  outfile="$(basename $infile .fq.gz)"_qc.fastq
  trimmomatic SE -phred33 -threads 2 $infile /workspace/trimmed/$outfile ILLUMINACLIP:adapters.fasta:2:30:10 LEADING:3 TRAILING:3 SLIDINGWINDOW:4:15 MINLEN:25
done

