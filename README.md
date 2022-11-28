# Scala JGSpec v2 to Dot

This project can transform JSON files according to the 
[JSON Graph Specification v2](https://github.com/jsongraph/json-graph-specification/blob/master/json-graph-schema_v2.json) to the DOT format 
programmatically in Scala. It supports simple graphs as well as hyper graphs. 

### How to use it

### Example

An exemplary Json file containing a simple graph with two nodes and one edge between them:

```JSON
{
  "graph": {
    "directed": false,
    "type": "graph type",
    "label": "graph label",
    "metadata": {
      "user-defined": "values"
    },
    "nodes": {
      "0": {
        "label": "node label(0)",
        "metadata": {
          "type": "node type",
          "user-defined": "values"
        }
      },
      "1": {
        "label": "node label(1)",
        "metadata": {
          "type": "node type",
          "user-defined": "values"
        }
      }
    },
    "edges": [
      {
        "source": "0",
        "relation": "edge relationship",
        "target": "1",
        "directed": false,
        "label": "edge label",
        "metadata": {
          "user-defined": "values"
        }
      }
    ]
  }
}
```
This JSON file can be processed to the Dot Representation by parsing it and afterwards transforming it
into the Dot Format by specifying how edges and nodes should be processed using an edge and node transformer. 
This project uses [Graph for Scala](https://www.scala-graph.org/) as an underlying engine to convert graphs to Dot files, their page has an excellent
documentation on how to create [edge and node transformers](https://www.scala-graph.org/guides/dot.html).

