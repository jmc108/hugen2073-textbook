(function () {
  const root = document.getElementById("tutor-root");
  if (!root) return;

  const chapter = root.dataset.chapter || "default";
  const storageKey = `tutor_chat_${chapter}`;
  const uiKey = `tutor_ui_${chapter}`;

  const state = {
    messages: JSON.parse(localStorage.getItem(storageKey) || "[]")
  };

  const ui = JSON.parse(localStorage.getItem(uiKey) || "null") || {
    width: 360,
    collapsed: false
  };

  function saveMessages() {
    localStorage.setItem(storageKey, JSON.stringify(state.messages));
  }
  function saveUI() {
    localStorage.setItem(uiKey, JSON.stringify(ui));
  }

  if (state.messages.length === 0) {
    state.messages.push({
      role: "assistant",
      content: `Do you have questions about ${chapter}, or do you want me to ask you the quiz questions?`
    });
    saveMessages();
  }

  root.innerHTML = `
    <div class="tutor-panel">
      <div class="tutor-header">
        <div class="tutor-title">Chat with Dr. Lysenko</div>
        <div class="tutor-controls" style="display:flex; gap:6px; align-items:center;">
          <button class="tutor-btn" id="tutor-toggle" type="button" title="Collapse/Expand">▾</button>
          <button class="tutor-btn" id="tutor-clear" type="button">Clear</button>
        </div>
      </div>

      <div class="tutor-history" id="tutor-history"></div>

      <div class="tutor-input">
        <textarea id="tutor-text" rows="2" placeholder="Ask a question…"></textarea>
        <button class="tutor-btn tutor-send" id="tutor-send" type="button">Send</button>
      </div>

      <div class="tutor-resize-handle" id="tutor-resize-handle" aria-hidden="true"></div>
    </div>
  `;

  const historyEl = document.getElementById("tutor-history");
  const textEl = document.getElementById("tutor-text");
  const toggleBtn = document.getElementById("tutor-toggle");

  function escapeHtml(s) {
    return String(s)
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll('"', "&quot;")
      .replaceAll("'", "&#039;");
  }

  function render() {
    historyEl.innerHTML = state.messages.map(m => `
      <div class="msg ${m.role}">
        <div class="bubble">${escapeHtml(m.content)}</div>
      </div>
    `).join("");
    historyEl.scrollTop = historyEl.scrollHeight;
  }

  async function send(content) {
    if (!content.trim()) return;

    state.messages.push({ role: "user", content });
    saveMessages(); render();

    state.messages.push({ role: "assistant", content: "…" });
    saveMessages(); render();

    const apiMessages = state.messages
      .filter(m => m.content !== "…")
      .map(m => ({ role: m.role, content: m.content }));

    let data;
    try {
      const res = await fetch("https://hugen-tutor-api.jonchernus.workers.dev", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          "x-tutor-secret": "hugen2071-dev-derp"
        },
        body: JSON.stringify({ chapter, messages: apiMessages })
      });
      data = await res.json();
    } catch (e) {
      data = { error: String(e) };
    }

    state.messages.pop();

    if (data && data.text) state.messages.push({ role: "assistant", content: data.text });
    else state.messages.push({ role: "assistant", content: `Error: ${data?.error || "No response"}` });

    saveMessages(); render();
  }

  document.getElementById("tutor-send").addEventListener("click", () => {
    const v = textEl.value;
    textEl.value = "";
    send(v);
  });

  document.getElementById("tutor-clear").addEventListener("click", () => {
    state.messages = [];
    saveMessages();
    render();
  });

  textEl.addEventListener("keydown", (e) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      document.getElementById("tutor-send").click();
    }
  });

  // ----- UI apply -----
  function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }

  function applyUI() {
    // allow a wide range: up to 80% viewport, capped at 900px
    const maxW = Math.min(900, Math.floor(window.innerWidth * 0.8));
    const w = clamp(ui.width || 360, 280, maxW);
    root.style.setProperty("--tutor-width", `${w}px`);

    root.classList.toggle("tutor-collapsed", !!ui.collapsed);
    toggleBtn.textContent = ui.collapsed ? "▸" : "▾";
  }

  applyUI();

  toggleBtn.addEventListener("click", () => {
    ui.collapsed = !ui.collapsed;
    saveUI();
    applyUI();
  });

  // ----- Resize (robust: listen on window) -----
  const handle = document.getElementById("tutor-resize-handle");
  let resizing = false;
  let startX = 0;
  let startW = 0;

  function onMove(e) {
    if (!resizing) return;
    // pulling left increases width
    const dx = startX - e.clientX;
    ui.width = startW + dx;
    applyUI();
    e.preventDefault();
  }

  function endResize() {
    if (!resizing) return;
    resizing = false;
    saveUI();
    window.removeEventListener("pointermove", onMove, true);
    window.removeEventListener("pointerup", endResize, true);
    window.removeEventListener("pointercancel", endResize, true);
  }

  handle.addEventListener("pointerdown", (e) => {
    // Ignore on small screens where you may not be floating
    if (!root.classList.contains("tutor-floating")) return;

    resizing = true;
    startX = e.clientX;
    startW = root.getBoundingClientRect().width;

    // Capture movement/up anywhere (fixes "sticky" behavior)
    window.addEventListener("pointermove", onMove, true);
    window.addEventListener("pointerup", endResize, true);
    window.addEventListener("pointercancel", endResize, true);

    e.preventDefault();
  });

  window.addEventListener("resize", () => {
    applyUI();
    saveUI();
  });

  render();
})();
