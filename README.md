# SeqStat


#### Mode - Generate

SeqStats outputs several stats on a FASTQ file.

Outputted stats:

- Bases
   - Total number
   - Base qualities, with the number of bases having that quality
   - Number of each nucleotide
- Reads
   - Total number
   - minimum length
   - maximum length
   - A histogram of the average base qualities
   - The quality encoding (Sanger, solexa etc.)
   - A histogram of the read lengths.
    
        

#### Mode - Merge

This module will merge seqstat files together and keep the sample/library/readgroup structure.
If required it's also possible to collapse this, the output file then des not have any sample/library/readgroup structure.
    
        

#### Mode - Validate

A file from SeqStat will validate the input files.
If aggregation values can not be regenerated the file is considered corrupt.
This should only happen when the user will edit the seqstat file manual.
    
        

# Documentation

For documentation and manuals visit our [github.io page](https://biopet.github.io/seqstat).

# About


SeqStat is part of BIOPET tool suite that is developed at LUMC by [the SASC team](http://sasc.lumc.nl/).
Each tool in the BIOPET tool suite is meant to offer a standalone function that can be used to perform a
dedicate data analysis task or added as part of [BIOPET pipelines](http://biopet-docs.readthedocs.io/en/latest/).

All tools in the BIOPET tool suite are [Free/Libre](https://www.gnu.org/philosophy/free-sw.html) and
[Open Source](https://opensource.org/osd) Software.
    

# Contact


<p>
  <!-- Obscure e-mail address for spammers -->
For any question related to SeqStat, please use the
<a href='https://github.com/biopet/seqstat/issues'>github issue tracker</a>
or contact
 <a href='http://sasc.lumc.nl/'>the SASC team</a> directly at: <a href='&#109;&#97;&#105;&#108;&#116;&#111;&#58;&#115;&#97;&#115;&#99;&#64;&#108;&#117;&#109;&#99;&#46;&#110;&#108;'>
&#115;&#97;&#115;&#99;&#64;&#108;&#117;&#109;&#99;&#46;&#110;&#108;</a>.
</p>

     

