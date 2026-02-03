//---LULC CLASSIFICATION 1995---//

// Defining study area
var gglm = ee.FeatureCollection("projects/ee-classificationlulc/assets/GreaterGiyani");
var studyarea = gglm;
print('studyarea:', gglm);
Map.centerObject(gglm, 10);
Map.addLayer(gglm, {color: 'red'}, 'gglm');
print(gglm.geometry().coordinates());

// LANDSAT 5 FOR 1995 //

// Defining time period for 1995
var startDateL5_1995 = '1995-01-01';
var endDateL5_1995 = '1995-12-31';

// Importing and filtering Landsat 5 for 1995
var landsat5_1995 = ee.ImageCollection("LANDSAT/LT05/C02/T1_L2")
  .filterDate(startDateL5_1995, endDateL5_1995)
  .filterBounds(gglm)
  .filter(ee.Filter.lt('CLOUD_COVER', 10));

// Imagery metadata for 1995
var numImages1995 = landsat5_1995.size();
var imageIds1995 = landsat5_1995.aggregate_array('system:id');
var dates1995 = landsat5_1995.aggregate_array('system:time_start');
print('1995 - Number of Images:', numImages1995);
print('1995 - Image IDs:', imageIds1995);
print('1995 - Image Dates:', dates1995);
print('1995 - Date Range:', landsat5_1995.aggregate_min('system:time_start'), 
      'to', landsat5_1995.aggregate_max('system:time_start'));

// Applying scaling factors
function applyScaleFactorsL5(image) {
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBand = image.select('ST_B6').multiply(0.00341802).add(149.0);
  return image.addBands(opticalBands, null, true)
              .addBands(thermalBand, null, true)
              .clip(gglm);
}
landsat5_1995 = landsat5_1995.map(applyScaleFactorsL5);

// Median composite for 1995
var medianImageL5_1995 = landsat5_1995.median().clip(gglm);

// Visualisation for 1995 (True Colour)
var visualizationL5 = {
  bands: ['SR_B3', 'SR_B2', 'SR_B1'],
  min: 0.0,
  max: 0.3,
};
Map.addLayer(medianImageL5_1995, visualizationL5, 'True Color 1995');

// TRAINING AND CLASSIFICATION 1995 //

// NDVI, NDBI, and NDWI
var ndvi1995 = medianImageL5_1995.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
var ndbi1995 = medianImageL5_1995.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDBI');
var ndwi1995 = medianImageL5_1995.normalizedDifference(['SR_B2', 'SR_B4']).rename('NDWI');
var inputL5_1995 = medianImageL5_1995.addBands([ndvi1995, ndbi1995, ndwi1995]);

// Defining bands
var bandsL5_1995 = ['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'NDVI', 'NDBI', 'NDWI'];

// Manually splitting each class into 175 training and 75 validation points
var builtUpTrain_1995 = builtUpL5_1995.limit(175, 'geometry');
var builtUpTest_1995 = builtUpL5_1995.filter(ee.Filter.inList('system:index', builtUpTrain_1995.aggregate_array('system:index')).not()).limit(75);

var agriculturalTrain_1995 = agriculturalL5_1995.limit(175, 'geometry');
var agriculturalTest_1995 = agriculturalL5_1995.filter(ee.Filter.inList('system:index', agriculturalTrain_1995.aggregate_array('system:index')).not()).limit(75);

var denseVegetationTrain_1995 = denseVegetationL5_1995.limit(175, 'geometry');
var denseVegetationTest_1995 = denseVegetationL5_1995.filter(ee.Filter.inList('system:index', denseVegetationTrain_1995.aggregate_array('system:index')).not()).limit(75);

var rangelandTrain_1995 = rangelandL5_1995.limit(175, 'geometry');
var rangelandTest_1995 = rangelandL5_1995.filter(ee.Filter.inList('system:index', rangelandTrain_1995.aggregate_array('system:index')).not()).limit(75);

var bareErodedTrain_1995 = bareErodedL5_1995.limit(175, 'geometry');
var bareErodedTest_1995 = bareErodedL5_1995.filter(ee.Filter.inList('system:index', bareErodedTrain_1995.aggregate_array('system:index')).not()).limit(75);

var waterTrain_1995 = waterL5_1995.limit(175, 'geometry');
var waterTest_1995 = waterL5_1995.filter(ee.Filter.inList('system:index', waterTrain_1995.aggregate_array('system:index')).not()).limit(75);

// Merging training and testing sets
var trainSetL5_1995 = builtUpTrain_1995
  .merge(agriculturalTrain_1995)
  .merge(denseVegetationTrain_1995)
  .merge(rangelandTrain_1995)
  .merge(bareErodedTrain_1995)
  .merge(waterTrain_1995);

var testSetL5_1995 = builtUpTest_1995
  .merge(agriculturalTest_1995)
  .merge(denseVegetationTest_1995)
  .merge(rangelandTest_1995)
  .merge(bareErodedTest_1995)
  .merge(waterTest_1995);

print('Training Set Size 1995:', trainSetL5_1995.size()); 
print('Validation Set Size 1995:', testSetL5_1995.size()); 

// Sampling image to add band values to training and validation sets
var trainImageL5_1995 = inputL5_1995.select(bandsL5_1995).sampleRegions({
  collection: trainSetL5_1995,
  properties: ['Class'],
  scale: 30
});
var testImageL5_1995 = inputL5_1995.select(bandsL5_1995).sampleRegions({
  collection: testSetL5_1995,
  properties: ['Class'],
  scale: 30
});

// Training classifier for 1995
var classifierL5_1995 = ee.Classifier.smileRandomForest(300).train({
  features: trainImageL5_1995,
  classProperty: 'Class',
  inputProperties: bandsL5_1995
});
var classifiedL5_1995 = inputL5_1995.classify(classifierL5_1995);
var trainCounts1995 = trainSetL5_1995.aggregate_histogram('Class');
var testCounts1995 = testSetL5_1995.aggregate_histogram('Class');
print('Training Points per Class 1995:', trainCounts1995);
print('Validation Points per Class 1995:', testCounts1995);

// TRAINING AND VALIDATION POINTS 1995 //

print('Built-up 1995 Points:', builtUpL5_1995.size());
print('Agricultural 1995 Points:', agriculturalL5_1995.size());
print('Dense Vegetation 1995 Points:', denseVegetationL5_1995.size());
print('Rangeland 1995 Points:', rangelandL5_1995.size());
print('Bare/eroded 1995 Points:', bareErodedL5_1995.size());
print('Water 1995 Points:', waterL5_1995.size());

// Visualisation for 1995 (Classification)
var landcoverVis = {
  min: 1,
  max: 6,
  palette: ['#808080', '#c51ab6', '228b22', '#cffbb0', '#f3ac50', '#0000ff']
};
Map.addLayer(classifiedL5_1995, landcoverVis, 'LULC Classification 1995');

// Class distribution verification for 1995
var classAreas = classifiedL5_1995.reduceRegion({
  reducer: ee.Reducer.frequencyHistogram(),
  geometry: gglm,
  scale: 30,
  maxPixels: 1e13
});
print('Class Distribution 1995:', classAreas);

// ACCURACY ASSESSMENT 1995 //

var testClassificationL5_1995 = testImageL5_1995.classify(classifierL5_1995);
var confusionMatrixL5_1995 = testClassificationL5_1995.errorMatrix('Class', 'classification');
print('Confusion Matrix 1995:', confusionMatrixL5_1995);
print('Overall Accuracy 1995:', confusionMatrixL5_1995.accuracy());
print('Kappa Coefficient 1995:', confusionMatrixL5_1995.kappa());

// FEATURE IMPORTANCE //

var importance1995 = classifierL5_1995.explain();
print('Feature Importance 1995:', importance1995);

// POST-PROCESSING FOR 1995 //

//  Minimal majority filter (2x2 kernel)
var kernel = ee.Kernel.square({
  radius: 0.5, 
  units: 'pixels'
});

var smoothed1995 = classifiedL5_1995.reduceNeighborhood({
  reducer: ee.Reducer.mode(),
  kernel: kernel
}).rename('classification');

// Spectral Indices for Corrections
var ndvi1995 = medianImageL5_1995.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
var ndbi1995 = medianImageL5_1995.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDBI');
var ndwi1995 = medianImageL5_1995.normalizedDifference(['SR_B2', 'SR_B4']).rename('NDWI');
var mndwi1995 = medianImageL5_1995.normalizedDifference(['SR_B2', 'SR_B5']).rename('MNDWI'); 
var sr_b7 = medianImageL5_1995.select('SR_B7');
var sr_b3 = medianImageL5_1995.select('SR_B3');

// Automated Spectral Rules
var corrected1995 = smoothed1995;

// Rule 1: Agricultural (2) → Bare/eroded (5) where NDVI < 0.3, SR_B7 > 0.25, SR_B3 > 0.1
var agriToBare = corrected1995.eq(2).and(ndvi1995.lt(0.3)).and(sr_b7.gt(0.25)).and(sr_b3.gt(0.1));
corrected1995 = corrected1995.where(agriToBare, 5);

// Rule 2: Water (6) → Dense Vegetation (3) where NDWI > 0.1 and NDVI > 0.4
var waterToVeg = corrected1995.eq(6).and(ndwi1995.gt(0.1)).and(ndvi1995.gt(0.4));
corrected1995 = corrected1995.where(waterToVeg, 3);

// Rule 3: Agricultural (2) → Rangeland (4) where NDVI < 0.2 and SR_B7 > 0.3
var agriToRange = corrected1995.eq(2).and(ndvi1995.lt(0.2)).and(sr_b7.gt(0.3));
corrected1995 = corrected1995.where(agriToRange, 4);

// Rule 4: Dense Vegetation (3) → Agricultural (2) where NDVI < 0.4 and SR_B7 > 0.25
var vegToAgri = corrected1995.eq(3).and(ndvi1995.lt(0.4)).and(sr_b7.gt(0.25));
corrected1995 = corrected1995.where(vegToAgri, 2);

// Rule 5: Bare/eroded (5) → Built-up (1) where NDBI > 0.2 and SR_B3 < 0.1
var bareToBuiltup = corrected1995.eq(5).and(ndbi1995.gt(0.3)).and(sr_b3.lt(0.1)).and(ndwi1995.lt(0.1));
corrected1995 = corrected1995.where(bareToBuiltup, 1);

// Rule 6: Protect Water (6) where NDWI > 0.15
var protectWater = corrected1995.neq(6).and(ndwi1995.gt(0.05).or(mndwi1995.gt(0.1)));
corrected1995 = corrected1995.where(protectWater, 6);

// Rule 7: Dense Vegetation (3) → Rangeland (4) where NDVI < 0.35 and SR_B7 > 0.3
var vegToRange = corrected1995.eq(3).and(ndvi1995.lt(0.35)).and(sr_b7.gt(0.3));
corrected1995 = corrected1995.where(vegToRange, 4);

// Manual Corrections for Specific Regions 
var manuallyCorrected1995 = corrected1995;

manuallyCorrected1995 = manuallyCorrected1995.where(
  corrected1995.eq(2).and(corrected1995.clip(agriToVegfix)), 3
);
manuallyCorrected1995 = manuallyCorrected1995.where(
  corrected1995.eq(2).and(corrected1995.clip(agriToBarefix)), 5
);
manuallyCorrected1995 = manuallyCorrected1995.where(
  corrected1995.eq(6).and(corrected1995.clip(waterToVegfix)), 3
);
manuallyCorrected1995 = manuallyCorrected1995.where(
  corrected1995.eq(6).and(corrected1995.clip(waterToBuiltupfix)), 1
);
manuallyCorrected1995 = manuallyCorrected1995.where(
  corrected1995.eq(2).and(corrected1995.clip(agriToRangefix)), 4
);
manuallyCorrected1995 = manuallyCorrected1995.where(
  corrected1995.eq(3).and(corrected1995.clip(vegToAgrifix)), 2
);
manuallyCorrected1995 = manuallyCorrected1995.where(
  corrected1995.eq(5).and(corrected1995.clip(bareToWaterfix)), 6 
);
manuallyCorrected1995 = manuallyCorrected1995.where(
  corrected1995.eq(6).and(corrected1995.clip(waterToBarefix)), 5 
);
manuallyCorrected1995 = manuallyCorrected1995.where(
  corrected1995.eq(1).and(corrected1995.clip(builtupToBarefix)), 5 
);
manuallyCorrected1995 = manuallyCorrected1995.where(
  corrected1995.eq(4).and(corrected1995.clip(rangeToVegfix)), 3 
);

// Check for zero pixels
var zeroPixels = manuallyCorrected1995.eq(0).reduceRegion({
  reducer: ee.Reducer.sum(),
  geometry: gglm,
  scale: 30,
  maxPixels: 1e13
});
print('Zero Pixels in 1995:', zeroPixels); 

// CORRECTED ACCURACY ASSESSMENT 1995 //

var testSetL5_1995 = testSetL5_1995.filter(ee.Filter.and(ee.Filter.gte('Class', 1), ee.Filter.lte('Class', 6)));
var testAfter = manuallyCorrected1995.sampleRegions({
  collection: testSetL5_1995,
  properties: ['Class'],
  scale: 30
});
var cmAfter = testAfter.errorMatrix('Class', 'classification');
print('Corrected Accuracy :', cmAfter.accuracy());
print('Corrected Kappa :', cmAfter.kappa());
print('Confusion Matrix (Corrected) :', cmAfter);

// Corrected Visualisation
Map.addLayer(manuallyCorrected1995, landcoverVis, 'Corrected 1995 (After)');
var changedPixels = smoothed1995.neq(manuallyCorrected1995);
Map.addLayer(changedPixels, {min: 0, max: 1, palette: ['white', 'red']}, 'Corrected Pixels');

// Corrected Class Distribution
var correctedClassAreas = manuallyCorrected1995.reduceRegion({
  reducer: ee.Reducer.frequencyHistogram(),
  geometry: gglm,
  scale: 30,
  maxPixels: 1e13
});
print('Corrected Class Distribution (1995):', correctedClassAreas);

// Function to reproject 
function prepareImage(image) {
  return image.reproject({
    crs: 'EPSG:32736',
    scale: 30
  }).clip(gglm).int(); 
}

// Prepare corrected image
var corrected1995 = prepareImage(manuallyCorrected1995);

// Export Corrected LULC Map for 1995
Export.image.toDrive({
  image: corrected1995,
  description: 'Corrected_LULC_1995',
  folder: 'GEE_Exports',
  region: gglm,
  scale: 30,
  crs: 'EPSG:32736',
  maxPixels: 1e13
});

// Export Corrected Confusion Matrix for 1995
Export.table.toDrive({
  collection: ee.FeatureCollection(cmAfter),
  description: 'Corrected_ConfusionMatrix_1995_Final',
  folder: 'GEE_Exports',

  //---LULC CLASSIFICATION 2000---//

  // Defining study area
var gglm = ee.FeatureCollection("projects/ee-classificationlulc/assets/GreaterGiyani");
var studyarea = gglm;
print('studyarea:', gglm);
Map.centerObject(gglm, 10);
Map.addLayer(gglm, {color: 'red'}, 'gglm');
print(gglm.geometry().coordinates());

// LANDSAT 5 FOR 2000 //

// Defining time period
var startDateL5_2000 = '2000-01-01';
var endDateL5_2000 = '2000-12-31';

// Importing and filtering Landsat 5 for 2000
var landsat5_2000 = ee.ImageCollection("LANDSAT/LT05/C02/T1_L2")
  .filterDate(startDateL5_2000, endDateL5_2000)
  .filterBounds(gglm)
  .filter(ee.Filter.lt('CLOUD_COVER', 10));

// Imagery metadata for 2000
var numImages2000 = landsat5_2000.size();
var imageIds2000 = landsat5_2000.aggregate_array('system:id');
var dates2000 = landsat5_2000.aggregate_array('system:time_start');
print('2000 - Number of Images:', numImages2000);
print('2000 - Image IDs:', imageIds2000);
print('2000 - Image Dates:', dates2000);
print('2000 - Date Range:', landsat5_2000.aggregate_min('system:time_start'), 
      'to', landsat5_2000.aggregate_max('system:time_start'));
      
// Applying scaling factors
function applyScaleFactorsL5(image) {
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBand = image.select('ST_B6').multiply(0.00341802).add(149.0);
  return image.addBands(opticalBands, null, true)
              .addBands(thermalBand, null, true)
              .clip(gglm);
}
landsat5_2000 = landsat5_2000.map(applyScaleFactorsL5);

// Median composite for 2000
var medianImageL5_2000 = landsat5_2000.median().clip(gglm);

// Visualisation for 2000 (True Colour)
var visualizationL5 = {
  bands: ['SR_B3', 'SR_B2', 'SR_B1'],
  min: 0.0,
  max: 0.3,
};
Map.addLayer(medianImageL5_2000, visualizationL5, 'True Color 2000');

// TRAINING AND CLASSIFICATION 2000 //

// NDVI, NDBI, and NDWI
var ndvi2000 = medianImageL5_2000.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
var ndbi2000 = medianImageL5_2000.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDBI');
var ndwi2000 = medianImageL5_2000.normalizedDifference(['SR_B2', 'SR_B4']).rename('NDWI');
var inputL5_2000 = medianImageL5_2000.addBands([ndvi2000, ndbi2000, ndwi2000]);

// Defining bands
var bandsL5_2000 = ['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'NDVI', 'NDBI', 'NDWI'];

// Training and validation points
var builtUpL5_2000 = ee.FeatureCollection('projects/ee-classificationlulc/assets/BuiltUpL5_2000');
var agriculturalL5_2000 = ee.FeatureCollection('projects/ee-classificationlulc/assets/AgriculturalL5_2000');
var denseVegetationL5_2000 = ee.FeatureCollection('projects/ee-classificationlulc/assets/DenseVegetationL5_2000');
var rangelandL5_2000 = ee.FeatureCollection('projects/ee-classificationlulc/assets/RangelandL5_2000');
var bareErodedL5_2000 = ee.FeatureCollection('projects/ee-classificationlulc/assets/BareErodedL5_2000');
var waterL5_2000 = ee.FeatureCollection('projects/ee-classificationlulc/assets/WaterL5_2000');


// Manually splitting each class into 175 training and 75 validation points
var builtUpTrain_2000 = builtUpL5_2000.limit(175, 'geometry');
var builtUpTest_2000 = builtUpL5_2000.filter(ee.Filter.inList('system:index', builtUpTrain_2000.aggregate_array('system:index')).not()).limit(75);

var agriculturalTrain_2000 = agriculturalL5_2000.limit(175, 'geometry');
var agriculturalTest_2000 = agriculturalL5_2000.filter(ee.Filter.inList('system:index', agriculturalTrain_2000.aggregate_array('system:index')).not()).limit(75);

var denseVegetationTrain_2000 = denseVegetationL5_2000.limit(175, 'geometry');
var denseVegetationTest_2000 = denseVegetationL5_2000.filter(ee.Filter.inList('system:index', denseVegetationTrain_2000.aggregate_array('system:index')).not()).limit(75);

var rangelandTrain_2000 = rangelandL5_2000.limit(175, 'geometry');
var rangelandTest_2000 = rangelandL5_2000.filter(ee.Filter.inList('system:index', rangelandTrain_2000.aggregate_array('system:index')).not()).limit(75);

var bareErodedTrain_2000 = bareErodedL5_2000.limit(175, 'geometry');
var bareErodedTest_2000 = bareErodedL5_2000.filter(ee.Filter.inList('system:index', bareErodedTrain_2000.aggregate_array('system:index')).not()).limit(75);

var waterTrain_2000 = waterL5_2000.limit(175, 'geometry');
var waterTest_2000 = waterL5_2000.filter(ee.Filter.inList('system:index', waterTrain_2000.aggregate_array('system:index')).not()).limit(75);

// Merging training and testing sets
var trainSetL5_2000 = builtUpTrain_2000
  .merge(agriculturalTrain_2000)
  .merge(denseVegetationTrain_2000)
  .merge(rangelandTrain_2000)
  .merge(bareErodedTrain_2000)
  .merge(waterTrain_2000);

var testSetL5_2000 = builtUpTest_2000
  .merge(agriculturalTest_2000)
  .merge(denseVegetationTest_2000)
  .merge(rangelandTest_2000)
  .merge(bareErodedTest_2000)
  .merge(waterTest_2000);

print('Training Set Size 2000:', trainSetL5_2000.size()); 
print('Validation Set Size 2000:', testSetL5_2000.size()); 

// Sampling image to add band values to training and validation sets
var trainImageL5_2000 = inputL5_2000.select(bandsL5_2000).sampleRegions({
  collection: trainSetL5_2000,
  properties: ['Class'],
  scale: 30
});

var testImageL5_2000 = inputL5_2000.select(bandsL5_2000).sampleRegions({
  collection: testSetL5_2000,
  properties: ['Class'],
  scale: 30
});

// Training classifier for 2000
var classifierL5_2000 = ee.Classifier.smileRandomForest(300).train({
  features: trainImageL5_2000,
  classProperty: 'Class',
  inputProperties: bandsL5_2000
});
var classifiedL5_2000 = inputL5_2000.classify(classifierL5_2000);
var trainCounts2000 = trainSetL5_2000.aggregate_histogram('Class');
var testCounts2000 = testSetL5_2000.aggregate_histogram('Class');
print('Training Points per Class 2000:', trainCounts2000);
print('Validation Points per Class 2000:', testCounts2000);

// TRAINING AND VALIDATION POINTS 2000 //

print('Built-up 2000 Points:', builtUpL5_2000.size());
print('Agricultural 2000 Points:', agriculturalL5_2000.size());
print('Dense Vegetation 2000 Points:', denseVegetationL5_2000.size());
print('Rangeland 2000 Points:', rangelandL5_2000.size());
print('Bare/eroded 2000 Points:', bareErodedL5_2000.size());
print('Water 2000 Points:', waterL5_2000.size());

// Visualisation for 2000 (Classification)
var landcoverVis = {
  min: 1,
  max: 6,
  palette: ['#808080', '#c51ab6', '228b22', '#cffbb0', '#f3ac50', '#0000ff']
};
Map.addLayer(classifiedL5_2000, landcoverVis, 'LULC Classification 2000');

// Class distribution verification for 2000
var classAreas = classifiedL5_2000.reduceRegion({
  reducer: ee.Reducer.frequencyHistogram(),
  geometry: gglm,
  scale: 30,
  maxPixels: 1e13
});
print('Class Distribution 2000:', classAreas);

// ACCURACY ASSESSMENT 2000 //

var testClassificationL5_2000 = testImageL5_2000.classify(classifierL5_2000);
var confusionMatrixL5_2000 = testClassificationL5_2000.errorMatrix('Class', 'classification');
print('Confusion Matrix 2000:', confusionMatrixL5_2000);
print('Overall Accuracy 2000:', confusionMatrixL5_2000.accuracy());
print('Kappa Coefficient 2000:', confusionMatrixL5_2000.kappa());

// FEATURE IMPORTANCE //

var importance2000 = classifierL5_2000.explain();
print('Feature Importance 2000:', importance2000);

// POST-PROCESSING FOR 2000 //

// Minimal majority filter (2x2 kernel)
var kernel = ee.Kernel.square({
  radius: 0.5, 
  units: 'pixels'
});

var smoothed2000 = classifiedL5_2000.reduceNeighborhood({
  reducer: ee.Reducer.mode(),
  kernel: kernel
}).rename('classification');

// Spectral Indices for Corrections
var ndvi2000 = medianImageL5_2000.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
var ndbi2000 = medianImageL5_2000.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDBI');
var ndwi2000 = medianImageL5_2000.normalizedDifference(['SR_B2', 'SR_B4']).rename('NDWI');
var mndwi2000 = medianImageL5_2000.normalizedDifference(['SR_B2', 'SR_B5']).rename('MNDWI');
var sr_b3 = medianImageL5_2000.select('SR_B3');
var sr_b7 = medianImageL5_2000.select('SR_B7');

// Automated Spectral Rules
var corrected2000 = smoothed2000;

// Rule 1: Water (6) → Dense Vegetation (3) where NDWI > 0.1 and NDVI > 0.4
var waterToVeg = corrected2000.eq(6).and(ndwi2000.gt(0.1)).and(ndvi2000.gt(0.4));
corrected2000 = corrected2000.where(waterToVeg, 3);

// Rule 2: Water (6) → Built-up (1) where NDWI < -0.1 and NDBI > 0
var waterToBuiltup = corrected2000.eq(6).and(ndwi2000.lt(-0.2)).and(ndbi2000.gt(0.3));
corrected2000 = corrected2000.where(waterToBuiltup, 1);

// Rule 3: Dense Vegetation (3) → Agricultural (2) where NDVI < 0.4 and SR_B7 > 0.25
var vegToAgri = corrected2000.eq(3).and(ndvi2000.lt(0.4)).and(sr_b7.gt(0.25));
corrected2000 = corrected2000.where(vegToAgri, 2);

// Rule 4: Bare eroded (5) → Built-up (1) where NDBI < 0.3 and SR_B3 > 0.1
var bareToBuiltup = corrected2000.eq(5).and(ndbi2000.gt(0.3)).and(sr_b3.lt(0.1)).and(ndwi2000.lt(0.1));
corrected2000 = corrected2000.where(bareToBuiltup, 1);

// Rule 5: Rangeland (4) → Dense vegetation (3)
var rangeToVeg = corrected2000.eq(4).and(ndbi2000.gt(0.55)).and(sr_b7.lt(0.2));
corrected2000 = corrected2000.where(bareToBuiltup, 3);

// Rule 6: Water (6) preservation
var waterPres = corrected2000.neq(6).and(ndwi2000.gt(0.15).or(mndwi2000.gt(0.1))).and(sr_b7.lt(0.3));
corrected2000 = corrected2000.where (waterPres, 6);

// Rule 7: Agricultural (2) → Rangeland (4) where NDVI < 0.25 and SR_B7 > 0.35
var agriToRange = corrected2000.eq(2).and(ndvi2000.lt(0.25)).and(sr_b7.gt(0.35));
corrected2000 = corrected2000.where(agriToRange, 4);

// Rule 8: Built-up (1) → Bare/eroded (5) where NDBI < 0.1 and SR_B3 > 0.15
var builtupToBare = corrected2000.eq(1).and(ndbi2000.lt(0.1)).and(sr_b3.gt(0.15));
corrected2000 = corrected2000.where(builtupToBare, 5);

// Manual Corrections for Specific Regions 
var manuallyCorrected2000 = corrected2000;

manuallyCorrected2000 = manuallyCorrected2000.where(
  corrected2000.eq(2).and(corrected2000.clip(agriToVegfix)), 3
);
manuallyCorrected2000 = manuallyCorrected2000.where(
  corrected2000.eq(2).and(corrected2000.clip(agriToBarefix)), 5
);
manuallyCorrected2000 = manuallyCorrected2000.where(
  corrected2000.eq(6).and(corrected2000.clip(waterToVegfix)), 3
);
manuallyCorrected2000 = manuallyCorrected2000.where(
  corrected2000.eq(2).and(corrected2000.clip(agriToRangefix)), 4
);
manuallyCorrected2000 = manuallyCorrected2000.where(
  corrected2000.eq(2).and(corrected2000.clip(agriToBuiltupfix)), 1
);
manuallyCorrected2000 = manuallyCorrected2000.where(
  corrected2000.eq(3).and(corrected2000.clip(vegToAgrifix)), 2
);
manuallyCorrected2000 = manuallyCorrected2000.where(
  corrected2000.eq(1).and(corrected2000.clip(builtToAgrifix)), 2
);
manuallyCorrected2000 = manuallyCorrected2000.where(
  corrected2000.eq(6).and(corrected2000.clip(waterToAgrifix)), 2
);
manuallyCorrected2000 = manuallyCorrected2000.where(
  corrected2000.eq(4).and(corrected2000.clip(rangeToAgrifix)), 2
);

manuallyCorrected2000 = manuallyCorrected2000.where(
  corrected2000.eq(5).and(corrected2000.clip(bareToBuiltupfix)), 1
);

// Check for zero pixels
var zeroPixels = manuallyCorrected1995.eq(0).reduceRegion({
  reducer: ee.Reducer.sum(),
  geometry: gglm,
  scale: 30,
  maxPixels: 1e13
});
print('Zero Pixels in 2000:', zeroPixels); 

// CORRECTED ACCURACY ASSESSMENT 2000 //

var testAfter = manuallyCorrected2000.sampleRegions({
  collection: testSetL5_2000,
  properties: ['Class'],
  scale: 30
});
var cmAfter = testAfter.errorMatrix('Class', 'classification');
print('Corrected Accuracy :', cmAfter.accuracy());
print('Corrected Kappa :', cmAfter.kappa());
print('Confusion Matrix (Corrected) :', cmAfter);

// Corrected Visualisation
Map.addLayer(manuallyCorrected2000, landcoverVis, 'Corrected 2000 (After)');
var changedPixels = smoothed2000.neq(manuallyCorrected2000);
Map.addLayer(changedPixels, {min: 0, max: 1, palette: ['white', 'red']}, 'Corrected Pixels');

// Corrected Class Distribution
var correctedClassAreas = manuallyCorrected2000.reduceRegion({
  reducer: ee.Reducer.frequencyHistogram(),
  geometry: gglm,
  scale: 30,
  maxPixels: 1e13
});
print('Corrected Class Distribution (2000):', correctedClassAreas);

// Function to reproject 
function prepareImage(image) {
  return image.reproject({
    crs: 'EPSG:32736',
    scale: 30
  }).clip(gglm).int(); 
}

// Prepare corrected image
var corrected2000 = prepareImage(manuallyCorrected2000);

// Export Corrected LULC Map for 2000
Export.image.toDrive({
  image: corrected2000,
  description: 'Corrected_LULC_2000',
  folder: 'GEE_Exports',
  region: gglm,
  scale: 30,
  crs: 'EPSG:32736',
  maxPixels: 1e13
});

// Export Corrected Confusion Matrix for 2000
Export.table.toDrive({
  collection: ee.FeatureCollection(cmAfter),
  description: 'Corrected_ConfusionMatrix_2000_Final',
  folder: 'GEE_Exports',
  fileFormat: 'CSV'
});


//---LULC CLASSIFICATION 2005---//

// Defining study area
var gglm = ee.FeatureCollection("projects/ee-classificationlulc/assets/GreaterGiyani");
var studyarea = gglm;
print('studyarea:', gglm);
Map.centerObject(gglm, 10);
Map.addLayer(gglm, {color: 'red'}, 'gglm');

// LANDSAT 5 FOR 2005 //

// Defining time period for 2005
var startDateL5_2005 = '2005-01-01';
var endDateL5_2005 = '2005-12-31';

// Importing and filtering Landsat 5 for 2005
var landsat5_2005 = ee.ImageCollection("LANDSAT/LT05/C02/T1_L2")
  .filterDate(startDateL5_2005, endDateL5_2005)
  .filterBounds(gglm)
  .filter(ee.Filter.lt('CLOUD_COVER', 10));
  
// Imagery metadata for 2005  
var numImages2005 = landsat5_2005.size();
var imageIds2005 = landsat5_2005.aggregate_array('system:id');
var dates2005 = landsat5_2005.aggregate_array('system:time_start');
print('2005 - Number of Images:', numImages2005);
print('2005 - Image IDs:', imageIds2005);
print('2005 - Image Dates:', dates2005);
print('2005 - Date Range:', landsat5_2005.aggregate_min('system:time_start'), 
      'to', landsat5_2005.aggregate_max('system:time_start'));
  
// Applying scaling factors
function applyScaleFactorsL5(image) {
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBand = image.select('ST_B6').multiply(0.00341802).add(149.0);
  return image.addBands(opticalBands, null, true)
              .addBands(thermalBand, null, true)
              .clip(gglm);
}
landsat5_2005 = landsat5_2005.map(applyScaleFactorsL5);

// Median composite for 2005
var medianImageL5_2005 = landsat5_2005.median().clip(gglm);

// Visualisation for 2005 (True Colour)
var visualizationL5 = {
  bands: ['SR_B3', 'SR_B2', 'SR_B1'],
  min: 0.0,
  max: 0.3,
};
Map.addLayer(medianImageL5_2005, visualizationL5, 'True Color 2005');

// TRAINING AND CLASSIFICATION 2005 //

// NDVI, NDBI, and NDWI (2005)
var ndvi2005 = medianImageL5_2005.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
var ndbi2005 = medianImageL5_2005.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDBI');
var ndwi2005 = medianImageL5_2005.normalizedDifference(['SR_B2', 'SR_B4']).rename('NDWI');
var inputL5_2005 = medianImageL5_2005.addBands([ndvi2005, ndbi2005, ndwi2005]);

// Defining bands for classification (including indices for 2005)
var bandsL5_2005 = ['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'NDVI', 'NDBI', 'NDWI'];

// Training and validation points
var builtUpL5_2005 = ee.FeatureCollection('projects/ee-classificationlulc/assets/BuiltUpL5_2005');
var agriculturalL5_2005 = ee.FeatureCollection('projects/ee-classificationlulc/assets/AgriculturalL5_2005');
var denseVegetationL5_2005 = ee.FeatureCollection('projects/ee-classificationlulc/assets/DenseVegetationL5_2005');
var rangelandL5_2005 = ee.FeatureCollection('projects/ee-classificationlulc/assets/RangelandL5_2005');
var bareErodedL5_2005 = ee.FeatureCollection('projects/ee-classificationlulc/assets/BareErodedL5_2005');
var waterL5_2005 = ee.FeatureCollection('projects/ee-classificationlulc/assets/WaterL5_2005');

// Manually splitting each class into 175 training and 75 validation points
var builtUpTrain_2005 = builtUpL5_2005.limit(175, 'geometry');
var builtUpTest_2005 = builtUpL5_2005.filter(ee.Filter.inList('system:index', builtUpTrain_2005.aggregate_array('system:index')).not()).limit(75);

var agriculturalTrain_2005 = agriculturalL5_2005.limit(175, 'geometry');
var agriculturalTest_2005 = agriculturalL5_2005.filter(ee.Filter.inList('system:index', agriculturalTrain_2005.aggregate_array('system:index')).not()).limit(75);

var denseVegetationTrain_2005 = denseVegetationL5_2005.limit(175, 'geometry');
var denseVegetationTest_2005 = denseVegetationL5_2005.filter(ee.Filter.inList('system:index', denseVegetationTrain_2005.aggregate_array('system:index')).not()).limit(75);

var rangelandTrain_2005 = rangelandL5_2005.limit(175, 'geometry');
var rangelandTest_2005 = rangelandL5_2005.filter(ee.Filter.inList('system:index', rangelandTrain_2005.aggregate_array('system:index')).not()).limit(75);

var bareErodedTrain_2005 = bareErodedL5_2005.limit(175, 'geometry');
var bareErodedTest_2005 = bareErodedL5_2005.filter(ee.Filter.inList('system:index', bareErodedTrain_2005.aggregate_array('system:index')).not()).limit(75);

var waterTrain_2005 = waterL5_2005.limit(175, 'geometry');
var waterTest_2005 = waterL5_2005.filter(ee.Filter.inList('system:index', waterTrain_2005.aggregate_array('system:index')).not()).limit(75);

// Merge training and testing sets
var trainSetL5_2005 = builtUpTrain_2005
  .merge(agriculturalTrain_2005)
  .merge(denseVegetationTrain_2005)
  .merge(rangelandTrain_2005)
  .merge(bareErodedTrain_2005)
  .merge(waterTrain_2005);

var testSetL5_2005 = builtUpTest_2005
  .merge(agriculturalTest_2005)
  .merge(denseVegetationTest_2005)
  .merge(rangelandTest_2005)
  .merge(bareErodedTest_2005)
  .merge(waterTest_2005);

print('Training Set Size 2005:', trainSetL5_2005.size()); 
print('Validation Set Size 2005:', testSetL5_2005.size()); 

// Sampling image to add band values to training and validation sets
var trainImageL5_2005 = inputL5_2005.select(bandsL5_2005).sampleRegions({
  collection: trainSetL5_2005,
  properties: ['Class'],
  scale: 30
});

var testImageL5_2005 = inputL5_2005.select(bandsL5_2005).sampleRegions({
  collection: testSetL5_2005,
  properties: ['Class'],
  scale: 30
});

// Training classifier for 2005
var classifierL5_2005 = ee.Classifier.smileRandomForest(300).train({
  features: trainImageL5_2005,
  classProperty: 'Class',
  inputProperties: bandsL5_2005
});
var classifiedL5_2005 = inputL5_2005.classify(classifierL5_2005);
var trainCounts2005 = trainSetL5_2005.aggregate_histogram('Class');
var testCounts2005 = testSetL5_2005.aggregate_histogram('Class');
print('Training Points per Class 2005:', trainCounts2005);
print('Validation Points per Class 2005:', testCounts2005);

// TRAINING AND VALIDATION POINTS 2005 //

print('Built-up 2005 Points:', builtUpL5_2005.size());
print('Agricultural 2005 Points:', agriculturalL5_2005.size());
print('Dense Vegetation 2005 Points:', denseVegetationL5_2005.size());
print('Rangeland 2005 Points:', rangelandL5_2005.size());
print('Bare/eroded 2005 Points:', bareErodedL5_2005.size());
print('Water 2005 Points:', waterL5_2005.size());

// Visualisation for 2005 (Classification)
var landcoverVis = {
  min: 1,
  max: 6,
  palette: ['#808080', '#c51ab6', '228b22', '#cffbb0', '#f3ac50', '#0000ff']
};
Map.addLayer(classifiedL5_2005, landcoverVis, 'LULC Classification 2005');

// Class distribution verification for 2005
var classAreas = classifiedL5_2005.reduceRegion({
  reducer: ee.Reducer.frequencyHistogram(),
  geometry: gglm,
  scale: 30,
  maxPixels: 1e10
});
print('Class Distribution 2005:', classAreas);

// ACCURACY ASSESSMENT 2005 //

var testClassificationL5_2005 = testImageL5_2005.classify(classifierL5_2005);
var confusionMatrixL5_2005 = testClassificationL5_2005.errorMatrix('Class', 'classification');
print('Confusion Matrix 2005:', confusionMatrixL5_2005);
print('Overall Accuracy 2005:', confusionMatrixL5_2005.accuracy());
print('Kappa Coefficient 2005:', confusionMatrixL5_2005.kappa());

// FEATURE IMPORTANCE //

var importance2005 = classifierL5_2005.explain();
print('Feature Importance 2005:', importance2005);

// POST-PROCESSING FOR 2005 //

// Minimal majority filter (2x2 kernel)
var kernel = ee.Kernel.square({
  radius: 0.5, 
  units: 'pixels'
});

var smoothed2005 = classifiedL5_2005.reduceNeighborhood({
  reducer: ee.Reducer.mode(),
  kernel: kernel
}).rename('classification');

// Spectral Indices for Corrections
var ndvi2005 = medianImageL5_2005.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
var ndbi2005 = medianImageL5_2005.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDBI');
var ndwi2005 = medianImageL5_2005.normalizedDifference(['SR_B2', 'SR_B4']).rename('NDWI');
var mndwi2005 = medianImageL5_2005.normalizedDifference(['SR_B2', 'SR_B5']).rename('MNDWI');
var sr_b7 = medianImageL5_2005.select('SR_B7');
var sr_b3 = medianImageL5_2005.select('SR_B3');

// Automated Spectral Rules
var corrected2005 = smoothed2005;

// Rule 1: Dense Vegetation (3) → Water (6) where NDWI > 0.2 and NDVI < 0.4
var vegToWater = corrected2005.eq(3).and(ndwi2005.gt(0.25)).and(ndvi2005.lt(0.4));
corrected2005 = corrected2005.where(vegToWater, 6);

// Rule 2: Built-up (1) → Bare/eroded (5) where NDBI < 0.1 and SR_B3 > 0.15
var builtupToBare = corrected2005.eq(1).and(ndbi2005.lt(0.05)).and(sr_b3.gt(0.15));
corrected2005 = corrected2005.where(builtupToBare, 5);

// Rule 3: Dense Vegetation (3) → Rangeland (4) where NDVI < 0.35 and SR_B7 > 0.3
var vegToRange = corrected2005.eq(3).and(ndvi2005.lt(0.35)).and(sr_b7.gt(0.3));
corrected2005 = corrected2005.where(vegToRange, 4);

// Rule 4: Water (6) → Dense Vegetation (3) where NDWI > 0.2 and NDVI > 0.4
var waterToVeg = corrected2005.eq(6).and(ndwi2005.gt(0.2)).and(ndvi2005.gt(0.4)).and(mndwi2005.lt(0.2));
corrected2005 = corrected2005.where(waterToVeg, 3); 

// Rule 5: Agricultural (2) → Rangeland (4) where NDVI < 0.25 and SR_B7 > 0.35
var agriToRange = corrected2005.eq(2).and(ndvi2005.lt(0.2)).and(sr_b7.gt(0.3));
corrected2005 = corrected2005.where(agriToRange, 4);

// Rule 6: Protect Water (6) where NDWI > 0.15
var protectWater = corrected2005.neq(6).and(ndwi2005.gt(0.15).or(mndwi2005.gt(0.1))).and(sr_b7.lt(0.3));
corrected2005 = corrected2005.where(protectWater, 6);

// Rule 7: Bare/eroded (5) → Built-up (1) where NDBI > 0.3 and SR_B3 < 0.1
var bareToBuiltup = corrected2005.eq(5).and(ndbi2005.gt(0.05)).and(sr_b3.lt(0.1)).and(ndwi2005.lt(0.1));
corrected2005 = corrected2005.where(bareToBuiltup, 1); 

// Rule 8: Agricultural (2) → Built-up (1) where NDBI > 0.3 and NDVI < 0.3
var agriToBuiltup = corrected2005.eq(2).and(ndbi2005.gt(0.05)).and(ndvi2005.lt(0.3));
corrected2005 = corrected2005.where(agriToBuiltup, 1);

// Manual Corrections for Specific Regions
var manuallyCorrected2005 = corrected2005;

manuallyCorrected2005 = manuallyCorrected2005.where(
  corrected2005.eq(4).and(corrected2005.clip(rangeToBuiltupfix)), 1
);
manuallyCorrected2005 = manuallyCorrected2005.where(
  corrected2005.eq(6).and(corrected2005.clip(waterToVegfix)), 3
);
manuallyCorrected2005 = manuallyCorrected2005.where(
  corrected2005.eq(2).and(corrected2005.clip(agriToVegfix)), 3
);
manuallyCorrected2005 = manuallyCorrected2005.where(
  corrected2005.eq(6).and(corrected2005.clip(waterToAgrifix)), 2
);
manuallyCorrected2005 = manuallyCorrected2005.where(
  corrected2005.eq(1).and(corrected2005.clip(builtupToBarefix)), 5
);
manuallyCorrected2005 = manuallyCorrected2005.where(
  corrected2005.eq(5).and(corrected2005.clip(bareToBuiltupfix)), 1
);
manuallyCorrected2005 = manuallyCorrected2005.where(
  corrected2005.eq(2).and(corrected2005.clip(agriToBuiltupfix)), 1
);
manuallyCorrected2005 = manuallyCorrected2005.where(
  corrected2005.eq(2).and(corrected2005.clip(agriToWaterfix)), 6
);
manuallyCorrected2005 = manuallyCorrected2005.where(
  corrected2005.eq(1).and(corrected2005.clip(builtupToRangefix)), 4
);
manuallyCorrected2005 = manuallyCorrected2005.where(
  corrected2005.eq(1).and(corrected2005.clip(builtupToAgrifix)), 2
);

// Check for zero pixels
var zeroPixels = manuallyCorrected2005.eq(0).reduceRegion({
  reducer: ee.Reducer.sum(),
  geometry: gglm,
  scale: 30,
  maxPixels: 1e13
});
print('Zero Pixels in 2005:', zeroPixels); 

// CORRECTED ACCURACY ASSESSMENT 2005 //

var testAfter = manuallyCorrected2005.sampleRegions({
  collection: testSetL5_2005,
  properties: ['Class'],
  scale: 30
});
var cmAfter = testAfter.errorMatrix('Class', 'classification');
print('Corrected Accuracy :', cmAfter.accuracy());
print('Corrected Kappa :', cmAfter.kappa());
print('Confusion Matrix (Corrected) :', cmAfter);

// Corrected Visualisation
Map.addLayer(manuallyCorrected2005, landcoverVis, 'Corrected 2005 (After)');
var changedPixels = smoothed2005.neq(manuallyCorrected2005);
Map.addLayer(changedPixels, {min: 0, max: 1, palette: ['white', 'red']}, 'Corrected Pixels');

// Corrected Class Distribution
var correctedClassAreas = manuallyCorrected2005.reduceRegion({
  reducer: ee.Reducer.frequencyHistogram(),
  geometry: gglm,
  scale: 30,
  maxPixels: 1e13
});
print('Corrected Class Distribution (2005):', correctedClassAreas);

// Function to reproject 
function prepareImage(image) {
  return image.reproject({
    crs: 'EPSG:32736',
    scale: 30
  }).clip(gglm).int(); 
}

// Prepare corrected image
var corrected2005 = prepareImage(manuallyCorrected2005);

// Export Corrected LULC Map for 2005
Export.image.toDrive({
  image: corrected2005,
  description: 'Corrected_LULC_2005',
  folder: 'GEE_Exports',
  region: gglm,
  scale: 30,
  crs: 'EPSG:32736',
  maxPixels: 1e13
});

// Export Corrected Confusion Matrix for 2005
Export.table.toDrive({
  collection: ee.FeatureCollection(cmAfter),
  description: 'Corrected_ConfusionMatrix_2005_Final',
  folder: 'GEE_Exports',
  fileFormat: 'CSV'
});


  fileFormat: 'CSV'
});
