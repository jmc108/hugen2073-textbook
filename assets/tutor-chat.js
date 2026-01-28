(function () {
  const root = document.getElementById("tutor-root");
  if (!root) return;

  const chapter = root.dataset.chapter || "default";
  const storageKey = `tutor_chat_${chapter}`;
  const uiKey = `tutor_ui_${chapter}`;

  const state = {
    messages: JSON.parse(localStorage.getItem(storageKey) || "[]")
  };

  // UI state (position/size/collapsed/hidden)
  const ui = JSON.parse(localStorage.getItem(uiKey) || "null") || {
    left: null,      // px
    top: null,       // px
    width: null,     // px
    height: null,    // px
    collapsed: false,
    hidden: false
  };

  function saveMessages() {
    localStorage.setItem(storageKey, JSON.stringify(state.messages));
  }

  function saveUI() {
    localStorage.setItem(uiKey, JSON.stringify(ui));
  }

  // If no prior history for this chapter, greet first
  if (state.messages.length === 0) {
    state.messages.push({
      role: "assistant",
      content: `Do you have questions about ${chapter}, or do you want me to ask you the quiz questions?`
    });
    saveMessages();
  }

  root.innerHTML = `
    <div class="tutor-panel">
      <div class="tutor-header" id="tutor-header">
        <div class="tutor-title">Chat with Dr. Lysenko</div>
        <div class="tutor-controls">
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
    </div>

    <div class="tutor-resizer" id="tutor-resizer" aria-hidden="true"></div>
  `;

  // Restore button (outside root, for hidden state)
  const restoreBtn = document.createElement("button");
  restoreBtn.className = "tutor-restore";
  restoreBtn.id = "tutor-restore";
  restoreBtn.type = "button";
  restoreBtn.textContent = "Tutor";
  document.body.appendChild(restoreBtn);

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

    // placeholder bubble
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

    // replace placeholder
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

  // Enter = send, Shift+Enter = newline
  textEl.addEventListener("keydown", (e) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      document.getElementById("tutor-send").click();
    }
  });

  // ---------------------------
  // Apply / manage UI state
  // ---------------------------
  function applyUI() {
    // collapsed/hidden classes
    root.classList.toggle("tutor-collapsed", !!ui.collapsed);
    root.classList.toggle("tutor-hidden", !!ui.hidden);

    restoreBtn.classList.toggle("visible", !!ui.hidden);

    // size
    if (ui.width) root.style.width = `${ui.width}px`;
    if (ui.height) root.style.height = `${ui.height}px`;

    // position: if user dragged before, pin with left/top (override right/top default)
    if (ui.left != null && ui.top != null) {
      root.style.right = "auto";
      root.style.bottom = "auto";
      root.style.left = `${ui.left}px`;
      root.style.top = `${ui.top}px`;
    }
  }

  function clamp(v, lo, hi) {
    return Math.max(lo, Math.min(hi, v));
  }

  function captureRectToUI() {
    const rect = root.getBoundingClientRect();
    ui.left = rect.left;
    ui.top = rect.top;
    ui.width = rect.width;
    ui.height = rect.height;
  }

  applyUI();

  // Collapse toggle
  document.getElementById("tutor-collapse").addEventListener("click", () => {
    ui.collapsed = !ui.collapsed;
    saveUI();
    applyUI();
  });

  // Hide (mostly remove) + restore
  document.getElementById("tutor-hide").addEventListener("click", () => {
    ui.hidden = true;
    saveUI();
    applyUI();
  });

  restoreBtn.addEventListener("click", () => {
    ui.hidden = false;
    saveUI();
    applyUI();
  });

  // ---------------------------
  // Drag to move (header)
  // ---------------------------
  const headerEl = document.getElementById("tutor-header");

  let drag = null;

  headerEl.addEventListener("pointerdown", (e) => {
    // Don’t start drag if clicking on a button inside header
    if (e.target.closest("button")) return;

    const rect = root.getBoundingClientRect();
    drag = {
      startX: e.clientX,
      startY: e.clientY,
      startLeft: rect.left,
      startTop: rect.top
    };

    root.setPointerCapture(e.pointerId);

    // switch to left/top anchoring
    root.style.right = "auto";
    root.style.bottom = "auto";
    root.style.left = `${rect.left}px`;
    root.style.top = `${rect.top}px`;
  });

  headerEl.addEventListener("pointermove", (e) => {
    if (!drag) return;

    const dx = e.clientX - drag.startX;
    const dy = e.clientY - drag.startY;

    const newLeft = drag.startLeft + dx;
    const newTop = drag.startTop + dy;

    const maxLeft = window.innerWidth - root.offsetWidth;
    const maxTop = window.innerHeight - root.offsetHeight;

    root.style.left = `${clamp(newLeft, 8, maxLeft - 8)}px`;
    root.style.top = `${clamp(newTop, 8, maxTop - 8)}px`;
  });

  headerEl.addEventListener("pointerup", () => {
    if (!drag) return;
    drag = null;
    captureRectToUI();
    saveUI();
  });

  // ---------------------------
  // Resize (bottom-right)
  // ---------------------------
  const resizerEl = document.getElementById("tutor-resizer");
  let resize = null;

  resizerEl.addEventListener("pointerdown", (e) => {
    const rect = root.getBoundingClientRect();
    resize = {
      startX: e.clientX,
      startY: e.clientY,
      startW: rect.width,
      startH: rect.height
    };
    root.setPointerCapture(e.pointerId);
    e.preventDefault();
  });

  resizerEl.addEventListener("pointermove", (e) => {
    if (!resize) return;

    const dx = e.clientX - resize.startX;
    const dy = e.clientY - resize.startY;

    const minW = 280;
    const minH = 220;
    const maxW = Math.min(window.innerWidth * 0.92, 650);
    const maxH = Math.min(window.innerHeight * 0.86, 900);

    const newW = clamp(resize.startW + dx, minW, maxW);
    const newH = clamp(resize.startH + dy, minH, maxH);

    root.style.width = `${newW}px`;
    root.style.height = `${newH}px`;
  });

  resizerEl.addEventListener("pointerup", () => {
    if (!resize) return;
    resize = null;
    captureRectToUI();
    saveUI();
  });

  // Keep within viewport on resize
  window.addEventListener("resize", () => {
    const rect = root.getBoundingClientRect();
    if (ui.left != null && ui.top != null) {
      const maxLeft = window.innerWidth - rect.width;
      const maxTop = window.innerHeight - rect.height;
      ui.left = clamp(rect.left, 8, maxLeft - 8);
      ui.top = clamp(rect.top, 8, maxTop - 8);
      saveUI();
      applyUI();
    }
  });

  render();
})();
