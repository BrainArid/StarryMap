pvalFisher = 0.025;
overlapThreshold = 0.50;

# Load and format the input.
input = read.csv("C:/Users/Tara/Documents/PhD/starry_map/d3_code/sample_csv/GSE48865_VS_GSE57872.csv", header = TRUE, sep = ",");
input[,"A.Module.Label"] = as.character(input[,"A.Module.Label"]);
input[,"B.Module.Label"] = as.character(input[,"B.Module.Label"]);
input[,"A.Module.Size"] = as.integer(input[,"A.Module.Size"]);
input[,"B.Module.Size"] = as.integer(input[,"B.Module.Size"]);
input[,"Size.Ratio"] = as.numeric(input[,"Size.Ratio"]);
input[,"Overlap"] = as.integer(input[,"Overlap"]);
input[,"FE.pValue"] = as.numeric(input[,"FE.pValue"]);

# Create the data frame for the output.
output = data.frame();
for (i in 1:nrow(input)) {
  row = input[i,];
	trueOverlap = as.numeric(row["Overlap"]);
	significantOverlap = as.numeric(min(row["A.Module.Size"], row["B.Module.Size"]) * overlapThreshold);
  if (trueOverlap >= significantOverlap && row["FE.pValue"] <= pvalFisher){
    labelA = row["A.Module.Label"];
    labelB = row["B.Module.Label"];
    weight = 0; 
    if (row["Size.Ratio"] >= 0.75){
      weight = 4;
    }
    else if(row["Size.Ratio"] < 0.75 && row["Size.Ratio"] >= 0.50){
      weight = 3;
    }
    else if(row["Size.Ratio"] < 0.50 && row["Size.Ratio"] >= 0.25){
      weight = 2;
    }
    else{
      weight = 1;
    }
    newVector = c(I(labelA), I(labelB), weight);
    names(newVector) = names(output);
    output = rbind(output, newVector);
    colnames(output) = c("source", "target", "value");
    output[,"source"] = as.character(output[,"source"]);
    output[,"target"] = as.character(output[,"target"]);
  }
}
write.table(output, "C:/Users/Tara/Documents/PhD/starry_map/d3_code/sample_csv/GSE48865_VS_GSE57872_sankey.csv", sep=",", row.names = FALSE);

