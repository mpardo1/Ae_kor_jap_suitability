import cdsapi

c = cdsapi.Client()

vars = [ "daily_maximum_near_surface_air_temperature",  "daily_minimum_near_surface_air_temperature",  "precipitation"]

for var in vars:
    dataset = "projections-cmip6"
    request = {
        "temporal_resolution": "monthly",
        "experiment": "ssp2_4_5",
        "variable": var,
        "model": "ec_earth3_cc",
        "month": [
            "01", "02", "03",
            "04", "05", "06",
            "07", "08", "09",
            "10", "11", "12"
        ],
        "year": ["2050"],
        "area": [80, -20, 30, 40]
    }

    target = "data/InputHanski/ESP/CMIP6/min_temp/CMIP6_245_EU_monthlu_min_" + var + ".zip"
    c.retrieve(dataset, request, target)
    
