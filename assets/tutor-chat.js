(function () {
  const root = document.getElementById("tutor-root");
  if (!root) return;

  const chapter = root.dataset.chapter || "default";
  const storageKey = `tutor_chat_${chapter}`;
  const uiKey = `tutor_ui_${chapter}`;

  const state = {
    messages: JSON.parse(localStorage.getItem(storageKey) || "[]")
  };

  // UI state: width + collapsed/hidden
  const ui = JSON.parse(localStorage.getItem(uiKey) || "null") || {
    width: 320,
    collapsed: false,
    hidden: false
  };

  function saveMessages() {
    localStorage.setItem(storageKey, JSON.stringify(state.messages));
  }

  function saveUI() {
    localStorage.setItem(uiKey, JSON.stringify(ui));
  }

  // greet first time per chapter
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
          <button class="tutor-btn" id="tutor-collapse" type="button" title="Collapse">▾</button>
          <button class="tutor-btn" id="tutor-hide" type="button" title="Hide">✕</button>
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

  // restore pill
  let restoreBtn = document.getElementById("tutor-restore");
  if (!restoreBtn) {
    restoreBtn = document.createElement("button");
    restoreBtn.className = "tutor-restore";
    restoreBtn.id = "tutor-restore";
    restoreBtn.type = "button";
    restoreBtn.textContent = "Tutor";
    document.body.appendChild(restoreBtn);
  }

  const historyEl = document.getElementById("tutor-history");
  const textEl = document.getElementById("tutor-text");

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
    saveMessages();
    render();

    state.messages.push({ role: "assistant", content: "…" });
    saveMessages();
    render();

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

    if (data && data.text) {
      state.messages.push({ role: "assistant", content: data.text });
    } else {
      state.messages.push({ role: "assistant", content: `Error: ${data?.error || "No response"}` });
    }

    saveMessages();
    render();
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

  // ---------------------------
  // UI apply (width/collapse/hide)
  // ---------------------------
  function clamp(v, lo, hi) {
    return Math.max(lo, Math.min(hi, v));
  }

  function applyUI() {
    // width via CSS variable so your existing right/top layout stays intact
    const w = clamp(ui.width || 320, 280, Math.min(650, Math.floor(window.innerWidth * 0.6)));
    root.style.setProperty("--tutor-width", `${w}px`);

    root.classList.toggle("tutor-collapsed", !!ui.collapsed);
    root.classList.toggle("tutor-hidden", !!ui.hidden);
    restoreBtn.classList.toggle("visible", !!ui.hidden);
  }

  applyUI();

  // collapse toggle
  document.getElementById("tutor-collapse").addEventListener("click", () => {
    ui.collapsed = !ui.collapsed;
    saveUI();
    applyUI();
  });

  // hide + restore
  document.getElementById("tutor-hide").addEventListener("click", () => {
    ui.hidden = true;
    saveUI();
    applyUI();
  });

  restoreBtn.addEventListener("click", () => {
    ui.hidden = false;
    ui.collapsed = false; // restoring should show the full widget
    saveUI();
    applyUI();
  });

  // ---------------------------
  // Resize: drag the LEFT EDGE handle
  // ---------------------------
  const handle = document.getElementById("tutor-resize-handle");
  let resizing = null;

  handle.addEventListener("pointerdown", (e) => {
    // only on desktop floating mode
    if (!root.classList.contains("tutor-floating")) return;

    const rect = root.getBoundingClientRect();
    resizing = {
      startX: e.clientX,
      startW: rect.width
    };

    root.setPointerCapture(e.pointerId);
    e.preventDefault();
  });

  handle.addEventListener("pointermove", (e) => {
    if (!resizing) return;

    // Dragging left handle: moving mouse left increases width
    const dx = resizing.startX - e.clientX;
    const newW = resizing.startW + dx;

    ui.width = newW;
    applyUI(); // live update while dragging
  });

  handle.addEventListener("pointerup", () => {
    if (!resizing) return;
    resizing = null;
    saveUI();
  });

  window.addEventListener("resize", () => {
    // re-clamp width
    applyUI();
    saveUI();
  });

  render();
})();
