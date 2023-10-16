## Landscape_intactness_models #

This repository includes data and `R` code to reproduce the analyses shown in the article:

**Integrating discrete and continuum models of landscape intactness best explains global mammal extinction risk**

_by J.P. Ramírez-Delgado, M. Di Marco, C.J. Johnson, J.E.M. Watson, H.L. Beyer, L. De Assis Barros, R. Pillay, and O. Venter_

In this article, we compare the ability of patch-matrix, continuum, and hybrid models of landscape intactness to explain the risk of extinction for terrestrial mammals on a global scale.

Users should have `R` version 4.2.3 or higher to execute the scripts.

`R` scripts have been tested on `RStudio` version 2023.03.0+386.

## Data ##

The `species_data.txt` serves as input for scripts 1 to 15 and includes the following variables:
- taxon_id: IUCN taxon ID for each species. 
- speciesName: Taxonomic name for each species.
- order: Taxonomic order for each species.
- patch_area: Proportion of each species’ range overlapping with low human footprint levels (HFP values <3) in 2000.
- change_patch_area: Difference in the proportional overlap between each species’ range and low HFP levels (HFP values <3) between 2000 and 2013.
- habitat_intactness_p5: 5th percentile of the values of habitat intactness within each species’ range.
- habitat_intactness_p10: 10th percentile of the values of habitat intactness within each species’ range.
- habitat_intactness_p50: 50th percentile of the values of habitat intactness within each species’ range.
- habitat_intactness_p90: 90th percentile of the values of habitat intactness within each species’ range.
- habitat_intactness_p95: 95th percentile of the values of habitat intactness within each species’ range.
- change_habitat_intactness_p5: Difference in the 5th percentile of the values of habitat intactness within each species’ range between 2000 and 2013.
- change_habitat_intactness_p10: Difference in the 10th percentile of the values of habitat intactness within each species’ range between 2000 and 2013.
- change_habitat_intactness_p50: Difference in the 50th percentile of the values of habitat intactness within each species’ range between 2000 and 2013.
- change_habitat_intactness_p90: Difference in the 90th percentile of the values of habitat intactness within each species’ range between 2000 and 2013.
- change_habitat_intactness_p95: Difference in the 95th percentile of the values of habitat intactness within each species’ range between 2000 and 2013.
- patch_intactness_p5: 5th percentile of the values of habitat intactness within relatively intact habitat patches (i.e., HFP values <3) within each species’ range.
- patch_intactness_p10: 10th percentile of the values of habitat intactness within relatively intact habitat patches (i.e., HFP values <3) within each species’ range.
- patch_intactness_p50: 50th percentile of the values of habitat intactness within relatively intact habitat patches (i.e., HFP values <3) within each species’ range.
- patch_intactness_p90: 90th percentile of the values of habitat intactness within relatively intact habitat patches (i.e., HFP values <3) within each species’ range.
- patch_intactness_p95: 95th percentile of the values of habitat intactness within relatively intact habitat patches (i.e., HFP values <3) within each species’ range.
- change_patch_intactness_p5: Difference in the 5th percentile of the values of habitat intactness within relatively intact habitat patches (i.e., HFP values <3) within each species’ range between 2000 and 2013.
- change_patch_intactness_p10: Difference in the 10th percentile of the values of habitat intactness within relatively intact habitat patches (i.e., HFP values <3) within each species’ range between 2000 and 2013.
- change_patch_intactness_p50: Difference in the 50th percentile of the values of habitat intactness within relatively intact habitat patches (i.e., HFP values <3) within each species’ range between 2000 and 2013.
- change_patch_intactness_p90: Difference in the 90th percentile of the values of habitat intactness within relatively intact habitat patches (i.e., HFP values <3) within each species’ range between 2000 and 2013.
- change_patch_intactness_p95: Difference in the 95th percentile of the values of habitat intactness within relatively intact habitat patches (i.e., HFP values <3) within each species’ range between 2000 and 2013.
- habitat_breadth: Number of habitat types used as suitable habitat by each species.
- range_size: Area in square kilometres of each species’ distribution.
- gestation_length: Length of time of fetal growth in days as a proxy for a species’ reproductive output.
-  weaning age: Age at which primary nutritional dependency on the mother ends and independent foraging begins in days as a proxy for each species’ reproductive onset.
- realm: Biogeographic realm in which each species has over 50% of its distribution.
-  criterionB: IUCN Red List Criterion B, with '1' representing species meeting the criterion and '0' representing species not meeting the criterion.
-  rl_cat: IUCN Red List category assigned to each species.
-  threatened_status: Threatened status of each species.
