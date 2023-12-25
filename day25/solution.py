import networkx as nx


def graph(lines):
    G = nx.DiGraph()
    for l in lines:
        k, vs =  l.strip().split(": ")
        for v in vs.split(" "):
            G.add_edge(k, v, capacity=1.0)
            G.add_edge(v, k, capacity=1.0)
    return G


def solve(G):
    nodes = G.nodes()
    for n1 in nodes:
        for n2 in nodes:
            if n1 != n2:
                cut_value, (p1, p2) = nx.minimum_cut(G, n1, n2)
                if cut_value == 3:
                    return len(p1) * len(p2)


if __name__ == "__main__":
    with open("input.txt") as f:
        lines = f.readlines()
    print(solve(graph(lines)))


def test_solve():
    data = """jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr""".split("\n")
    assert 54 == solve(graph(data))
