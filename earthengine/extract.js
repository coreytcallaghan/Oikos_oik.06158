// 1. Import Checklists
// 2. Import Datasets
// 3. Reduce Datasets
// 4. Export Checklists with more data

// constants
var BUFFER = 5000
var ERROR = 500
var REDUCTION_SCALE = 500
var EXPORT = true
var SMALL = true // export a small subset of the data

/// ///////////////////////
// 1. Import Checklists
var checklists = ee.FeatureCollection('ft:1VHko8wiaQAmRKfqQuY_oGjaO27OAtYoRbriTVMrO')
// for testing:
if (!EXPORT) checklists = checklists.limit(50)

var countries = ee.FeatureCollection('USDOS/LSIB/2013')
var australia = countries.filterMetadata('iso_alpha3', 'equals', 'AUS')
// remove the checklists that arent found on mainland australia
checklists = checklists.filterBounds(australia)

if (SMALL) checklists = checklists.limit(10000)

// then convert the points into rough circles
checklists = checklists.map(function (f) {
  return f.buffer(BUFFER, ERROR)
})

/// ///////////////////////
// 2. Import Datasets
var travel_time = ee.Image('Oxford/MAP/accessibility_to_cities_2015_v1_0')
var viirs_median = ee.ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG').select('avg_rad').median()
var evi_median = ee.ImageCollection('LANDSAT/LC8_L1T_32DAY_EVI').median()
var world_pop = ee.ImageCollection('CIESIN/GPWv4/population-density').median()

// crops is multinomial so it doesnt really make sense to find the sample stdDev of it
// var crops = ee.Image('USGS/GFSAD1000_V1')

// combine to one image
var composite = viirs_median.addBands([travel_time, evi_median, world_pop])

// reduce over the entire collection
var reducer = ee.Reducer.mean()// .combine(ee.Reducer.median(), null,true)
  .combine(ee.Reducer.sampleStdDev(), null, true)
  .combine(ee.Reducer.count(), null, true)

// this doesnt preserve columns in the feature collection :(
var rregion = function () {
  return checklists.map(function (f) {
    return f.set(composite.reduceRegion({
      geometry: f.geometry(),
      reducer: reducer,
      scale: REDUCTION_SCALE
    })).setGeometry()
  })
}

var rregions = function () {
  return composite.reduceRegions({
    collection: checklists,
    reducer: reducer,
    scale: REDUCTION_SCALE,
    tileScale: 16
  })
}

var output = rregion()

if (!EXPORT) print(output)

// var chart = ui.Chart.feature.histogram(output, 'avg_rad_mean', 15)
var export_table = function () {
  Export.table.toDrive({
    collection: output,
    description: 'ebird_rregion_10k',
    folder: 'ebird',
    fileNamePrefix: 'ebird_aus_urbanness_rregion_10k'
  })
}

if (EXPORT) export_table()
