// Assign default button values

function do_onclick() {
    var button1_value = 0;
    var button2_value = 0;
    var button3_value = 0;
    var button4_value = 0;
    var last_answer = 0;
    var last_hint = 0;
    const total_length = 35;

    const _codon = {'Ala': ['GCA','GCC','GCG','GCT'], 
                    'Arg': ['AGA','AGG','CGA','CGC','CGG','CGT'], 
                    'Asn': ['AAC','AAT','GAC','GAT'],
                    'Cys': ['TGC','TGT'],
                    'Gln': ['CAA','CAG'],
                    'Glu': ['GAA','GAG'],
                    'Gly': ['GGA','GGC','GGG','GGT'],
                    'His': ['CAC','CAT'],
                    'Ile': ['ATA','ATC','ATT'],
                    'Leu': ['CTA','CTC','CTG','CTT','TTA','TTG'],
                    'Lys': ['AAA','AAG'],
                    'Met': ['ATG'],
                    'Phe': ['TTC','TTT'],
                    'Pro': ['CCA','CCC','CCG','CCT'],
                    'Ser': ['AGC','AGT','TCA','TCC','TCG','TCT'],
                    'Stp': ['TAA','TAG','TGA'],
                    'Thr': ['ACA','ACC','ACG','ACT'],
                    'Trp': ['TGG'],
                    'Tyr': ['TAC','TAT'],
                    'Val': ['GTA','GTC','GTG','GTT']
                    }

    // Codon table:
    var Codon = ['GCA','GCC','GCG','GCT','AGA','AGG','CGA','CGC','CGG','CGT',
    'AAC','AAT','GAC','GAT','TGC','TGT','CAA','CAG','GAA','GAG','GGA','GGC',
    'GGG','GGT','CAC','CAT','ATA','ATC','ATT','CTA','CTC','CTG','CTT','TTA',
    'TTG','AAA','AAG','ATG','TTC','TTT','CCA','CCC','CCG','CCT','AGC','AGT',
    'TCA','TCC','TCG','TCT','TAA','TAG','TGA','ACA','ACC','ACG','ACT','TGG',
    'TAC','TAT','GTA','GTC','GTG','GTT']
    var AA = ['Ala','Ala','Ala','Ala','Arg','Arg','Arg','Arg','Arg','Arg','Asn',
    'Asn','Asp','Asp','Cys','Cys','Gln','Gln','Glu','Glu','Gly','Gly','Gly',
    'Gly','His','His','Ile','Ile','Ile','Leu','Leu','Leu','Leu','Leu','Leu',
    'Lys','Lys','Met','Phe','Phe','Pro','Pro','Pro','Pro','Ser','Ser','Ser',
    'Ser','Ser','Ser','Stp','Stp','Stp','Thr','Thr','Thr','Thr','Trp','Tyr',
    'Tyr','Val','Val','Val','Val']
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

