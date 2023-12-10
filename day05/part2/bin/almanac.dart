import 'dart:convert';
import 'dart:io';
import 'dart:math';

typedef Mapping = ({int start, int end, int offset});
typedef Range = ({int start, int end});

List<Mapping> nextMappings(Iterator<String> iterator) {
  var mappings = <Mapping>[];
  
  // Skip the heading
  if (iterator.moveNext()) {
    print('  ${iterator.current}');

    while (iterator.moveNext() && iterator.current.isNotEmpty) {
      var parts = iterator.current.split(' ').map((elem) => int.parse(elem)).toList();
      var entry = (start: parts[1], end: parts[1] + parts[2], offset: parts[0] - parts[1]);
      mappings.add(entry);
    }
  }

  return mappings;
}

List<Range> applyMapping(Range range, List<Mapping> mappings) {
  List<Range> remaining = <Range>[range];
  List<Range> result = <Range>[];
  for (Mapping mapping in mappings) {
    List<Range> nextRemaining = [];
    for (var range in remaining) {
      if (mapping.start <= range.start && range.end <= mapping.end) {
        // mapping covers entire range
        result.add((start: range.start + mapping.offset, end: range.end + mapping.offset));
      } else if (range.start < mapping.start && mapping.start < range.end && range.start < mapping.end && mapping.end < range.end) {
        // mapping covers the middle of the range
        nextRemaining.add((start: range.start, end: mapping.start - 1));
        nextRemaining.add((start: mapping.start + 1, end: range.end));
        result.add((start: mapping.start + mapping.offset, end: mapping.end + mapping.offset));
      } else if (range.start <= mapping.end && mapping.end <= range.end) {
        // mapping ends in the range
        nextRemaining.add((start: mapping.end + 1, end: range.end));
        result.add((start: range.start + mapping.offset, end: mapping.end + mapping.offset));
      } else if (range.start <= mapping.start && mapping.start <= range.end) {
        // mapping starts in the range
        nextRemaining.add((start: range.start, end: mapping.start - 1));
        result.add((start: mapping.start + mapping.offset, end: range.end + mapping.offset));
      } else {
        nextRemaining.add(range);
      }
    }
    remaining = nextRemaining;
  }
  result = result + remaining;
  return result;
}

void main(List<String> arguments) {
  if (arguments.length < 1) {
    print('Missing file argument');
    return;
  }
  print('Open file: ${arguments[0]}!');
  
  var file = File(arguments[0]);
  var iterator = file.readAsLinesSync().iterator;

  // First line contains the seeds
  iterator.moveNext();
  
  var seeds = <Range>[];
  var seedsText = iterator.current.split(":")[1].split(" ");
  seedsText.retainWhere((elem) => elem != "");
  for (var i = 0; i < seedsText.length; i = i + 2) {
    int start = int.parse(seedsText[i]);
    Range next = (start: start, end: start + int.parse(seedsText[i + 1]));
    seeds.add(next);
  }

  // Next line will be empty
  iterator.moveNext();
  print('Seeds ${seeds}');

  // Read the maps
  var mappings = nextMappings(iterator);
  while (mappings.isNotEmpty) {
    var nextSeeds = <Range>[];
    for (var seedRange in seeds) {
      nextSeeds = nextSeeds + applyMapping(seedRange, mappings);
    }
    print("  step: ${nextSeeds}");

    seeds = nextSeeds;
    mappings = nextMappings(iterator);
  }

  print('Final: ${seeds}');
  var minimum = seeds.map((range) => range.start).reduce(min);
  print('Min: ${minimum}');
}
