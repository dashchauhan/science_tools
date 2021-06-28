### Thank you for using the Flow Cytometry Statistical Analysis app!

Developed by: Avril Metcalfe-Roach

Contact email: armetcal@msl.ubc.ca

GitHub: https://github.com/armetcal/science_tools

Updated: 27 June 2021

---

### Introduction <a name="intro"></a>

As a complement to tools like FlowJo, this app provides an interactive environment through which summary plots, statistics, and tables may be generated from flow cytometry statistical output files.

The app is currently optimized for Attune instruments, but should work for any .csv summary file that includes the following columns: **Sample, Gate, %Gated**. If you would like your machine's output to be integrated, please send a sample output file to the email above.

**Please note:** In order to conserve server time, the app is currently set to sleep after 5 minutes of inactivity. **Any unsaved work will be lost.**

App comments and suggestions are always welcome!

---

### Table of Contents

1. [Sample Data](#sample)
2. [Analysis Options](#analysis)
   1. [Upload Statistics](#upload)
   2. [Remove Unwanted Samples](#remove)
   3. [Upload Metadata](#meta)
   4. [Isotype Controls](#iso)
   5. [Gating](#gate)
   6. [Plot Options](#plotoptions)
3. [Result Tabs](#results)
   1. [Plot](#plot)
   2. [Stats Table](#stats)
   3. [Metadata Table](#metatable)
   4. [Documentation](#documentation)

---

### **Sample Data** <a name="sample"></a>

Three sets of sample data are available on [GitHub](https://github.com/armetcal/science_tools/tree/main/Flow%20Cytometry%20Statistical%20Analysis), along with the open-source R script for the Shiny app.

1. **Single data file, isotypes, metadata in sample IDs:** This file, named *single_iso_metaoptionb_data.csv*, uses Metadata Option B to group the data into High and Low. This can be achieved by typing 'High,Low' or 'Low,High' (without quotations) in the corresponding text box. Isotypes are identified by the suffix ' iso', which is the default.
2. **Single data file, isotypes, metadata in separate file:** This file is identical to the previous, but cannot be grouped by sample ID. Metadata is uploaded via Option A. Several other metadata columns are available for faceting and clustering. The data file is named *single_iso_data.csv* and the metadata is named *single_iso_metadata.csv*.
3. **Two data files, no isotypes, metadata in separate file:** No isotypes are included. Both data files are uploaded together in step 1. The data files are named *double_no_iso_data_#.csv*, where # is 1 or 2; the metadata file is named *double_no_iso_metadata.csv*. Two problems with this dataset are purposely included:
    * R7 is used to name two distinct cell populations, which will cause an error if R7 is used to calculate % Target (output population). 
    * Sample names are duplicated between the two files. This will cause duplicate values to appear in the dataset (as seen in the [Stats Table](#stats) output), unless the file number is indicated by a *Batch* column in the metadata file (as done here). As the data files are numbered 1 and 2, 1 will always be uploaded before 2; therefore, the first file is labeled 'File 1' and the second is labeled 'File 2'. The file numbers MUST be correct for proper sample identification!

---

### **Analysis Options** <a name="analysis"></a>

### 1. Upload Statistics <a name="upload"></a>

One or more statistical .csv files may be uploaded. Please ensure the following conditions are met:

* Names of gates of interest are unique.
* The file is from an Attune machine OR uses a similar format:
    * .csv file type
    * Row 1 are column names; all subsequent rows contain data
    * Column names include **Sample**, **Gate** and **%Gated** (i.e. % of Parent).
* If multiple files are uploaded:
    * Names of gates of interest must be identical between files.
    * Reusing sample names is acceptable if no metadata is uploaded. 
        * If metadata is uploaded: reusing sample names is not recommended, but can be coerced if the file of origin is specified in the metadata file (see [Upload Metadata](#meta)).
    
### 2. Remove Unwanted Samples <a name="remove"></a>

Any samples dragged into the 'Remove' box will not be included in any outputs, allowing the user to remove controls and outliers. If the % Target population is not able to be calculated for a given sample, the sample will automatically be excluded. For example, if isotype controls are subtracted from each sample (see [Isotype Controls](#iso)), the single stain controls will not have isotypes to subtract and will therefore not be included.

### 3. Upload Metadata <a name="meta"></a>

Checking the *Metadata available?* box gives two options:

**Option A:** This allows the user to upload a single metadata .csv file. Any samples not included in the metadata sheet will not be in the output plot, but will be in the output tables as long as (Group, Facet, Cluster and other metadata columns will be blank - see [Result Tabs](#results)). Metadata can be checked in the *Metadata Table* tab.

After uploading, two sets of parameters will appear:
* The *Choose Variable of Interest* option. The user is then able to select the primary metadata column that will be used to separate the variables (i.e. the X axis of the plot).
* Under *Plot Options*, metadata can be used to facet the plot, cluster data subgroups, and set a statistical control group. Please see [Plot Options](#plotoptions) for more information.

*Note:* Reusing sample names with metadata is not recommended, but can be coerced if the file of origin is specified in the metadata file. The metadata column must be **Batch** and each input file must be labelled **File #**, where # indicates the file number. The file number MUST match the order in which the file is uploaded! An example of this is available in the ['double_no_iso' sample data](https://github.com/armetcal/science_tools/tree/main/Flow%20Cytometry%20Statistical%20Analysis) on GitHub.

**Option B:** This allows the user to specify a single grouping, as long as the grouping IDs are present in all sample names. Different group IDs are separated by commas (case sensitive; do not include spaces). The group ID can be anywhere in the sample name (Note: if isotype controls are used and identified by the sample name, they must be the sample suffix. Please see [Isotype Controls](#iso).)

For example, samples in the file single_iso_metaoptionb_data.csv have the following sample names: '# ID (iso)', where # is the sample number, ID denotes the group ID (High or Low), and the samples that have the suffix ' iso' are isotype controls. In the app, High and Low groups are then separated by typing *High,Low* or *Low,High* into the text box.

*Note:* The Metadata tab will be empty, as no metadata file was uploaded. Groups can be checked in the Stats Table tab.

### 4. Isotype Controls <a name="iso"></a>

Experiments that require an isotype (or background) control for each individual sample can be set up in this section. These controls will be processed normally, and the output (% Target) will be subtracted from the corresponding sample.

If ***Isotypes identified by suffix*** is selected (default), the sample names of the isotype control must be identical to the corresponding sample with the exception of a suffix. For example, if the sample is named **'Sample 1'**, the isotype control might be named **'Sample 1 iso'**, where ' iso' is the isotype identifier. ' iso' is the default suffix (space included).

If ***Isotypes identified by suffix*** is **not** selected, isotypes must be manually identified by sorting samples into the corresponding columns. Ensure that any samples that do not have isotype controls (such as other controls) are removed before sorting, as the Regular and Isotype columns must be the same length. Matching samples must appear in the same order.

### 5. Gating <a name="gate"></a>

This section specifies which gates should be used for the statistical analysis. The **gate(s) of interest** refer to the cell population of primary interest, while the **Background** gate(s) will be used to normalize the gate(s) of interest to a percentage (% Target). **The % Target is the value used for all plots and statistical analysis.**

If **Parent** is selected as the Background gate, then the % Target will be identical to the *%Gated* column in the input file ("% of Parent Gate"). 

If **any other gate(s)** is chosen, the % Target population will be defined using the following equation: 

```{r}
% Target = Gate/(Gate + Background) * 100%
```

If multiple gates are selected for either option, they will be combined before continuing with the analysis (with the exception of the Parent background gate, which cannot be combined). For example:

```{r}
Total background = Background 1 + Background 2
% Target = Gate/(Gate + Total Background) * 100%
```

This is useful in certain situations; for example, if all positive events from a single gate are desired, but the only data available is from a 2D plot that has been divided into quadrants.

*Note:* The app will not identify nonsensical gating. For example, setting the negative gate to *All Events* is nonsensical because *All Events* already includes the gate of interest (and thus the gate of interest will be represented twice in the denominator).

### Plot Options <a name="plotoptions"></a>

The basic plot output is a boxplot with jittered dots representing individual samples. Up to five plot settings will appear, based on the presence of metadata and set parameters:

**Filter out NAs:** Only relevant when metadata is available. When checked, this option removes any samples that have no data for the variable of interest (plot X axis). *Note:* If other metadata columns are used to facet or cluster data in the plot (see below), this option currently does NOT filter out samples with NAs present in these columns. This can be circumvented by removing these samples through [Remove Unwanted Samples](#remove).

**Specify control group:** Only appears when metadata is available. The drop-down list will show None, Multiple (default), and every unique data group (defined by the variable of interest). 

* **None:** No significance tests will be performed.
* **Multiple:** No individual data group acts as a control; instead, the resultant p value will describe whether there are any data groups that are significantly different from the other groups as a whole. Possible tests include the **ANOVA** and the **Kruskal-Wallis** test.
* **Specific data group:** A specific data group acts as the control against which all other data groups will be directly compared in a pairwise manner. Possible tests include the **T test** and the **Wilcoxon** test.

**Specify statistical test:** Only appears when metadata is available. This allows the user to choose between parametric and non-parametric significance tests.

* Control group = Multiple
    * Parametric: ANOVA
    * Non-parametric: Kruskal-Wallis
* Control group = Specific data group
    * Parametric: T test
    * Non-parametric: Wilcoxon
    
**Facet plot by metadata column:** Faceting divides the data by a given variable and creates a separate plot (with separate statistics) for each group. Faceting can be used in two situations:

1. Multiple data files uploaded: The data can be faceted by Batch (file number). 
    * *Note:* This can be performed without any metadata; however, the only way to test whether the results from each file are significantly different from each other is to supply the file number in a metadata file. Please see [Upload Metadata](#meta).
2. Metadata uploaded: Any metadata column can be used to facet the plot, unless the column is the variable of interest.

**Cluster by metadata column:** Only appears when metadata is available AND when the statistical control group is 'Multiple' or 'None'. Clustering further subdivides each grouping from the variable of interest and displays these subgroupings on the plot and in the output statistical tables. 

*Note:* If 'Multiple' is selected as the statistical control group, the p values of a clustered plot will reflect the variance between the subgroups of each individual group.

---

### **Result Tabs** <a name="results"></a>


### Plot <a name="plot"></a>

This tab shows up to three elements, all of which are downloadable:

1. **Boxplot of data.** The plot width will change depending on the size of the app window.
2. **Statistical Summary.** This table summarizes all significance tests performed. At a minimum, the table will show the X variable, p value, and the test performed. P values will match what is displayed in the plot. If p values are not displayed, the sample size is too small to perform the calculation.
   * If pairwise comparisons are performed, the table will include *Group* and *Ctrl Group*, indicating the two data groups involved in the calculation.
   * Faceted and Clustered plots will include *Facet* and *Group* columns, respectively, to indicate which data subgroup the p value belongs to.
       * For Clustered plots, the Group will be a subgroup of the X variable.
3. **Grouped Data.** This table provides key summary statistics, where each row represents one boxplot. Data groupings are specified by *Facet*, *Group*, and *Cluster*. Summary statistics inlcude the sample size (N), mean, median, and standard deviation.

### Stats Table <a name="stats"></a>

This tab provides an output of the **% Target** populations on a per-sample basis. Metadata, if available, is included. If isotype controls were included, two additional **% Target** columns (Regular and Isotype) are generated so that the sample and isotype percentages may be inspected individually.

### Metadata Table <a name="metatable"></a>

This table provides a searchable copy of the uploaded metadata file for easy perusal.

### Documentation <a name="documentation"></a>

Wow, so meta!