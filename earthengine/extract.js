// 1. Import Checklists
// 2. Import Datasets
// 3. Reduce Datasets
// 4. Export Checklists with more data

//var countries = ee.var

// constants
var BUFFER_SCALE = 5000

//////////////////////////
// 1. Import Checklists
var checklists = ee.FeatureCollection('ft:1VHko8wiaQAmRKfqQuY_oGjaO27OAtYoRbriTVMrO')
// for testing: 
//checklists = checklists.limit(50)

var countries = ee.FeatureCollection("USDOS/LSIB/2013");
var australia = countries.filterMetadata('iso_alpha3' , 'equals', 'AUS')
// remove the checklists that arent found on mainland australia
checklists = checklists.filterBounds(australia)


checklists = checklists.map(function (f) {
  return f.buffer(5000, 500)
})

//////////////////////////
// 2. Import Datasets
var travel_time = ee.Image('Oxford/MAP/accessibility_to_cities_2015_v1_0')
var viirs_median = ee.ImageCollection("NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG").select('avg_rad').median()
var evi_median = ee.ImageCollection("LANDSAT/LC8_L1T_32DAY_EVI").median()
var world_pop = ee.ImageCollection('CIESIN/GPWv4/population-density').median()

// crops is multinomial so it doesnt really make sense to find the sample stdDev of it
// var crops = ee.Image('USGS/GFSAD1000_V1')

// combine to one raster

var composite = viirs_median.addBands([travel_time, evi_median,  world_pop])

var output = composite.reduceRegions({
  collection: checklists,
  reducer: ee.Reducer.mean().combine(ee.Reducer.percentile([0, 5, 50, 95, 100]), null,true)
                            .combine(ee.Reducer.sampleStdDev(), null, true),
  scale: 100,
  tileScale: 16
})

// remove the geometry of all of the points, this reduces the output file size by an order of magnitude
output = output.map(function (f) {
  return f.setGeometry()
})

//var chart = ui.Chart.feature.histogram(output, 'avg_rad_mean', 15)
//print(chart)

//print(output.first())
//Map.addLayer(composite)
var export_table  = function () {
Export.table.toDrive({
  collection: output,
  description: 'ebird_australia_w_adhoc_urbanness_measures', 
  folder: 'ebird',
  fileNamePrefix: 'ebird_aus_urbanness_full'
})}
  

export_table()