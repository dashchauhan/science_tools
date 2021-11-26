// Assign default button values

function do_onclick() {
    var button1_value = 0;
    var button2_value = 0;
    var button3_value = 0;
    var button4_value = 0;
    var last_answer = 0;
    var last_hint = 0;
    const total_length = 35;

    // Codon table:
    var Codon = ['AAA', 'AAC', 'AAG', 'AAT', 'ACA', 'ACC', 'ACG',
        'ACT', 'AGA', 'AGC', 'AGG', 'AGT', 'ATA', 'ATC',
        'ATG', 'ATT', 'CAA', 'CAC', 'CAG', 'CAT', 'CCA',
        'CCC', 'CCG', 'CCT', 'CGA', 'CGC', 'CGG', 'CGT',
        'CTA', 'CTC', 'CTG', 'CTT', 'GAA', 'GAC', 'GAG',
        'GAT', 'GCA', 'GCC', 'GCG', 'GCT', 'GGA', 'GGC',
        'GGG', 'GGT', 'GTA', 'GTC', 'GTG', 'GTT', 'TAA',
        'TAC', 'TAG', 'TAT', 'TCA', 'TCC', 'TCG', 'TCT',
        'TGA', 'TGC', 'TGG', 'TGT', 'TTA', 'TTC', 'TTG', 'TTT']
    var AA = ['Lys', 'Asn', 'Lys', 'Asn', 'Thr', 'Thr', 'Thr', 'Thr', 'Arg',
        'Ser', 'Arg', 'Ser', 'Ile', 'Ile', 'Met', 'Ile', 'Gln', 'His', 'Gln',
        'His', 'Pro', 'Pro', 'Pro', 'Pro', 'Arg', 'Arg', 'Arg', 'Arg', 'Leu',
        'Leu', 'Leu', 'Leu', 'Glu', 'Asp', 'Glu', 'Asp', 'Ala', 'Ala', 'Ala',
        'Ala', 'Gly', 'Gly', 'Gly', 'Gly', 'Val', 'Val', 'Val', 'Val', 'Stp',
        'Tyr', 'Stp', 'Tyr', 'Ser', 'Ser', 'Ser', 'Ser', 'Stp', 'Cys', 'Trp',
        'Cys', 'Leu', 'Phe', 'Leu', 'Phe']
    var array2D = new Array(2);
    array2D[0] = Codon;
    array2D[1] = AA;
    var temp = []
    for(i = 0;i = AA.length-1; i++)
    {
        if(AA[i]=='Stp')
        {
            temp.append(i);
        }
    }
    console.log(temp);
    //var myJsonString = JSON.stringify(temp);
    //document.getElementById("myText").innerHTML = myJsonString;
}

