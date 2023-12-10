import 'dart:convert';
import 'dart:io';
import 'dart:math';

typedef Mapping = ({int source, int offset, int range});

List<Mapping> nextMappings(Iterator<String> iterator) {
  var mappings = <Mapping>[];
  
  // Skip the heading
  if (iterator.moveNext()) {
    print('  ${iterator.current}');

    while (iterator.moveNext() && iterator.current.isNotEmpty) {
      var parts = iterator.current.split(' ').map((elem) => int.parse(elem)).toList();
      var entry = (source: parts[1], offset: parts[0] - parts[1], range: parts[2]);
      mappings.add(entry);
    }
  }

  return mappings;
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
  var seedsText = iterator.current.split(":")[1].split(" ");
  seedsText.retainWhere((elem) => elem != "");
  var seeds = seedsText.map((elem) => int.parse(elem)).toList();

  // Next line will be empty
  iterator.moveNext();
  print('Seeds ${seeds}');

  // Read the maps
  var mappings = nextMappings(iterator);
  while (mappings.isNotEmpty) {
    for (var index = 0; index < seeds.length; index++) {
      for (var entry in mappings) {
        var dist = seeds[index] - entry.source;
        if (0 <= dist && dist <= entry.range) {
          seeds[index] = seeds[index] + entry.offset;
          break;
        }
      }
    }
    print('  Next: ${seeds}');
    mappings = nextMappings(iterator);
  }

  print('Final: ${seeds}');
  var minimum = seeds.reduce(min);
  print('Min: ${minimum}');
}
