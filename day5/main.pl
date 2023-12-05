:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(reif)).
:- use_module(library(lists)).

seeds([
	     seeds(Seeds),
	     seed_soil(SeedToSoil),
	     soil_fertilizer(SoilToFertilizer),
	     fertilizer_water(FertilizerToWater),
	     water_light(WaterToLight),
	     light_temperature(LightToTemperature),
	     temperature_humidity(TemperatureToHumidity),
	     humidity_location(HumidityToLocation)
	 ]) -->
    "seeds:",
    spaced_nums(Seeds),
    nl,
    "seed-to-soil map:", nl,
    transformation(SeedToSoil),
    nl,
    "soil-to-fertilizer map:", nl,
    transformation(SoilToFertilizer),
    nl,
    "fertilizer-to-water map:", nl,
    transformation(FertilizerToWater),
    nl,
    "water-to-light map:", nl,
    transformation(WaterToLight),
    nl,
    "light-to-temperature map:", nl,
    transformation(LightToTemperature),
    nl,
    "temperature-to-humidity map:", nl,
    transformation(TemperatureToHumidity),
    nl,
    "humidity-to-location map:", nl,
    transformation(HumidityToLocation).

seeds2([
	     seeds(Seeds),
	     seed_soil(SeedToSoil),
	     soil_fertilizer(SoilToFertilizer),
	     fertilizer_water(FertilizerToWater),
	     water_light(WaterToLight),
	     light_temperature(LightToTemperature),
	     temperature_humidity(TemperatureToHumidity),
	     humidity_location(HumidityToLocation)
	 ]) -->
    "seeds:",
    seeds_range(Seeds),
    nl,
    "seed-to-soil map:", nl,
    transformation(SeedToSoil),
    nl,
    "soil-to-fertilizer map:", nl,
    transformation(SoilToFertilizer),
    nl,
    "fertilizer-to-water map:", nl,
    transformation(FertilizerToWater),
    nl,
    "water-to-light map:", nl,
    transformation(WaterToLight),
    nl,
    "light-to-temperature map:", nl,
    transformation(LightToTemperature),
    nl,
    "temperature-to-humidity map:", nl,
    transformation(TemperatureToHumidity),
    nl,
    "humidity-to-location map:", nl,
    transformation(HumidityToLocation).

spaced_nums([]) --> nl.
spaced_nums([X|Xs]) -->
    " ", seq(NumCs),
    spaced_nums(Xs),
    { number_chars(X, NumCs) }.

seeds_range([]) --> nl.
seeds_range([X-Y|Xs]) -->
    " ", seq(XCs), " ", seq(YCs),
    seeds_range(Xs),
    {
	number_chars(X, XCs),
	number_chars(Y, YCs)
    }.

transformation([]) --> [].
transformation([transform(SourceRangeStart, DestinationRangeStart, RangeLength)|Xs]) -->
    seq(SRSCs), " ", seq(DRSCs), " ", seq(RLCs), nl,
    transformation(Xs),
    {
	number_chars(SourceRangeStart, SRSCs),
	number_chars(DestinationRangeStart, DRSCs),
	number_chars(RangeLength, RLCs)
    }.
    
nl --> "\n".
    

main(X) :-
    phrase_from_file(seeds(Data), "input"),
    member(seeds(Seeds), Data),
    maplist(seed_transform(Data), Seeds, Locations),
    list_min(Locations, X).

main2(X) :-
    phrase_from_file(seeds2(Data), "input"),
    member(seeds(Seeds), Data),
    maplist(seed_transform_range(Data), Seeds, X0),
    append(X0, X1),
    keysort(X1, X2),
    X2 = [X-_|_].
    

seed_transform(Maps, Seed, Out) :-
    member(seed_soil(T0), Maps),
    map_transform(T0, Seed, X1),
    member(soil_fertilizer(T1), Maps),
    map_transform(T1, X1, X2),
    member(fertilizer_water(T2), Maps),
    map_transform(T2, X2, X3),
    member(water_light(T3), Maps),
    map_transform(T3, X3, X4),
    member(light_temperature(T4), Maps),
    map_transform(T4, X4, X5),
    member(temperature_humidity(T5), Maps),
    map_transform(T5, X5, X6),
    member(humidity_location(T6), Maps),
    map_transform(T6, X6, Out).

map_transform(Map, Source, Dest) :-
    member(transform(DestinationStart, SourceStart, Length), Map),
    Source #>= SourceStart,
    Source #< SourceStart + Length,
    !,
    Dest #= (Source - SourceStart) + DestinationStart.

map_transform(_, X, X).

% Range fits entirely in transformation
map_transform_range(Map, Source, [Dest]) :-
    Source = RangeStart-RangeLength,
    member(transform(DestinationStart, SourceStart, Length), Map),
    RangeStart #>= SourceStart,
    RangeStart #< SourceStart + Length,    
    RangeStart + RangeLength #=< SourceStart + Length,
    !,
    DestStart #= (RangeStart - SourceStart) + DestinationStart,
    Dest = DestStart-RangeLength.

% Range doesn't fit entirely in transformation
map_transform_range(Map, Source, [Dest|Dest0]) :-
    Source = RangeStart-RangeLength,
    member(transform(DestinationStart, SourceStart, Length), Map),
    RangeStart #>= SourceStart,
    RangeStart #< SourceStart + Length,
    RangeStart + RangeLength #> SourceStart + Length,
    !,
    DestStart #= (RangeStart - SourceStart) + DestinationStart,
    Dest = DestStart-FirstRangeLength,

    NewRangeStart #= SourceStart + Length,
    FirstRangeLength #= NewRangeStart - RangeStart,
    RangeLength #= FirstRangeLength + NewRangeLength,
    map_transform_range(Map, NewRangeStart-NewRangeLength, Dest0).

% First part of range doesn't fit in any transformation
map_transform_range(Map, Source, Dest) :-
    Source = RangeStart-RangeLength,
    member(transform(_, SourceStart, _), Map),
    RangeStart #< SourceStart,
    RangeStart + RangeLength #> SourceStart,
    !,
    FirstRangeStart #= RangeStart,
    FirstRangeLength #= SourceStart - RangeStart,
    SecondRangeStart #= SourceStart,
    SecondRangeLength #= RangeLength - FirstRangeLength,
    map_transform_range(Map, FirstRangeStart-FirstRangeLength, Dest0),
    map_transform_range(Map, SecondRangeStart-SecondRangeLength, Dest1),
    append(Dest0, Dest1, Dest).

map_transform_range(_, X, [X]).

seed_transform_range(Maps, Seed, Out) :-
    member(seed_soil(T0), Maps),
    map_transform_range(T0, Seed, X2),
    member(soil_fertilizer(T1), Maps),
    maplist(map_transform_range(T1), X2, X3),
    append(X3, X4),
    member(fertilizer_water(T2), Maps),
    maplist(map_transform_range(T2), X4, X5),
    append(X5, X6),
    member(water_light(T3), Maps),
    maplist(map_transform_range(T3), X6, X7),
    append(X7, X8),
    member(light_temperature(T4), Maps),
    maplist(map_transform_range(T4), X8, X9),
    append(X9, X10),
    member(temperature_humidity(T5), Maps),
    maplist(map_transform_range(T5), X10, X11),
    append(X11, X12),
    member(humidity_location(T6), Maps),
    maplist(map_transform_range(T6), X12, X13),
    append(X13, Out).
