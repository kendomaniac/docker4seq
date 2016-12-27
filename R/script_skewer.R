# folder where fastq are and where to save data /data/scratch/1864p22
#5' adapter GTCTCGTGGGCTCGG 
#3' adapter TCGTCGGCAGCGTC
#R1 read_R1.fastq.gz
#R2 read_R2.fastq.gz
#the file return as info in skewer.info SE for R1 only or PE for R1 + R2 trimming
sudo docker pull docker.io/rcaloger/skewer1
sudo docker run -v /data/scratch/:/data/scratch -d docker.io/rcaloger/skewer1 sh /bin/trim2.sh /data/scratch/1864p22 GTCTCGTGGGCTCGG TCGTCGGCAGCGTC read_R1.fastq.gz read_R2.fastq.gz
#sudo docker run -v /data/scratch/:/data/scratch -d docker.io/rcaloger/skewer1 sh /bin/trim1.sh /data/scratch/1864p22 GTCTCGTGGGCTCGG TCGTCGGCAGCGTC read_R1.fastq.gz