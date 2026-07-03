import { useEffect, useMemo, useState } from "react";
import {
  ReactFlow,
  Background,
  Controls,
  MiniMap,
  useNodesState,
  useEdgesState,
} from "@xyflow/react";
import "@xyflow/react/dist/style.css";

function toFlow(block) {
  const nodes = (block?.nodes || []).map((n) => ({
    id: n.id,
    type: n.type || "default",
    position: n.position || { x: 0, y: 0 },
    data: {
      label: n.data?.label ?? n.label ?? n.id,
    },
  }));

  const edges = (block?.edges || []).map((e) => ({
    id: e.id,
    source: e.source,
    target: e.target,
    animated: e.animated ?? false,
  }));

  return { nodes, edges };
}

export default function App() {
  const [catalog, setCatalog] = useState(null);
  const [selectedBlock, setSelectedBlock] = useState(0);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);

  useEffect(() => {
    async function loadCatalog() {
      try {
        setLoading(true);
        setError("");

        const res = await fetch("/catalog_tree.json");
        if (!res.ok) {
          throw new Error(`Failed to load catalog_tree.json (${res.status})`);
        }

        const data = await res.json();
        setCatalog(data);
      } catch (err) {
        setError(err.message || "Failed to load graph data");
      } finally {
        setLoading(false);
      }
    }

    loadCatalog();
  }, []);

  const blocks = catalog?.blocks || [];

  useEffect(() => {
    if (!blocks.length) return;

    const block = blocks[selectedBlock] || blocks[0];
    const flow = toFlow(block);
    setNodes(flow.nodes);
    setEdges(flow.edges);
  }, [blocks, selectedBlock, setNodes, setEdges]);

  const currentBlock = useMemo(() => {
    return blocks[selectedBlock] || null;
  }, [blocks, selectedBlock]);

  if (loading) {
    return (
      <div style={{ padding: 24, fontFamily: "sans-serif" }}>
        Loading graph...
      </div>
    );
  }

  if (error) {
    return (
      <div style={{ padding: 24, fontFamily: "sans-serif", color: "crimson" }}>
        {error}
      </div>
    );
  }

  if (!catalog || !blocks.length) {
    return (
      <div style={{ padding: 24, fontFamily: "sans-serif" }}>
        No blocks found in catalog_tree.json
      </div>
    );
  }

  return (
    <div style={{ width: "100vw", height: "100vh", display: "flex", flexDirection: "column" }}>
      <div
        style={{
          padding: "12px 16px",
          borderBottom: "1px solid #e5e7eb",
          fontFamily: "sans-serif",
          display: "flex",
          alignItems: "center",
          gap: 12,
          flexWrap: "wrap",
        }}
      >
        <strong>Alma Catalog Graph</strong>

        <label>
          Block:&nbsp;
          <select
            value={selectedBlock}
            onChange={(e) => setSelectedBlock(Number(e.target.value))}
          >
            {blocks.map((block, idx) => (
              <option key={idx} value={idx}>
                {idx + 1} — {block.subject_area || "Unknown"}
              </option>
            ))}
          </select>
        </label>

        <span style={{ opacity: 0.7 }}>
          {currentBlock?.subject_area || "Unknown subject area"}
        </span>
      </div>

      <div style={{ flex: 1 }}>
        <ReactFlow
          nodes={nodes}
          edges={edges}
          onNodesChange={onNodesChange}
          onEdgesChange={onEdgesChange}
          fitView
        >
          <MiniMap />
          <Controls />
          <Background gap={16} />
        </ReactFlow>
      </div>
    </div>
  );
}