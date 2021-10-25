# LoxVM

## GraphViz functionality

To use GraphViz to visualize the code output, use the following command:

```sh
echo 'print 1;' \
    | cargo run 2>&1 \
    | grep -Uo 'digraph[\s\S]+(\{[\s\S]*\})+' \
    > graph.dot ; \
    dot graph.dot -Tpng \
    > graph.png
```
