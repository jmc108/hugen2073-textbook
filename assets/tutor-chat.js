(function () {
  const root = document.getElementById("tutor-root");
  if (!root) return;

  const chapter = root.dataset.chapter;
  const storageKey = `tutor_chat_${chapter}`;

  const state = {
    messages: JSON.parse(localStorage.getItem(storageKey) || "[]")
  };

// If no prior history for this chapter, have the tutor greet first
if (state.messages.length === 0) {
  state.messages.push({
    role: "assistant",
    content: `Hi! I’m your chapter tutor for ${chapter}. What part of this chapter should we work on first?`
  });
  localStorage.setItem(storageKey, JSON.stringify(state.messages));
}


  root.innerHTML = `
    <div class="tutor-panel">
      <div class="tutor-header">
        <div class="tutor-title">Chapter Tutor</div>
        <button class="tutor-btn" id="tutor-clear" type="button">Clear</button>
      </div>

      <div class="tutor-history" id="tutor-history"></div>

      <div class="tutor-input">
        <textarea id="tutor-text" rows="2" placeholder="Ask a question…"></textarea>
        <button class="tutor-btn tutor-send" id="tutor-send" type="button">Send</button>
      </div>

      <div class="tutor-footer">
        <button class="tutor-btn" id="tutor-quiz" type="button">Quiz me</button>
      </div>
    </div>
  `;

  const historyEl = document.getElementById("tutor-history");
  const textEl = document.getElementById("tutor-text");

  function save() {
    localStorage.setItem(storageKey, JSON.stringify(state.messages));
  }

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
    save();
    render();

    // placeholder bubble
    state.messages.push({ role: "assistant", content: "…" });
    save();
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

    save();
    render();
  }

  document.getElementById("tutor-send").addEventListener("click", () => {
    const v = textEl.value;
    textEl.value = "";
    send(v);
  });

  document.getElementById("tutor-quiz").addEventListener("click", () => {
    send("Quiz me with 4 short concept-check questions based on this chapter. Ask one at a time.");
  });

  document.getElementById("tutor-clear").addEventListener("click", () => {
    state.messages = [];
    save();
    render();
  });

  // Enter = send, Shift+Enter = newline
  textEl.addEventListener("keydown", (e) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      document.getElementById("tutor-send").click();
    }
  });

  render();
})();

