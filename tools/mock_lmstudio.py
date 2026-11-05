#!/usr/bin/env python3
"""A stand-in for LM Studio, for browser checks without a real model.
Answers any chat request with a fixed coach or gap answer. Usage: mock_lmstudio.py [port] [delay_s]"""
import json, sys, time
from http.server import BaseHTTPRequestHandler, HTTPServer

DELAY = float(sys.argv[2]) if len(sys.argv) > 2 else 0.2
COACH = {"criteria": {"singular": "pass", "verifiable": "pass", "design_independent": "question", "conflicts": "none", "fits_scope": "pass"},
         "questions": [{"text": "What happens if the power system can't reach the flight controller?", "context": "Mock answer."}],
         "example_rewrite": "When the bus drops, the power system shall report its status within 9 s.", "conflicts": []}
GAPS = {"items": [
    {"kind": "question", "title": "What keeps the battery inside its temperature range?", "body": "Mock answer.", "target": "", "confidence": "medium", "name": "", "stereotype": "", "draft": "", "ports": []},
    {"kind": "sketch_element", "title": "What notices if the Flight Controller stops?", "body": "Mock answer.", "target": "", "confidence": "medium", "name": "Watchdog 5000", "stereotype": "HW", "draft": "", "ports": []},
]}

class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args): pass
    def send_json(self, value):
        body = json.dumps(value).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)
    def do_GET(self):
        self.send_json({"data": [{"id": "mock-model"}]})
    def do_POST(self):
        request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        time.sleep(DELAY)
        text = json.dumps(request.get("messages", ""))
        answer = COACH
        if "answer questions about a system model" in text:
            messages = request.get("messages", [])
            if not any(m.get("role") == "tool" for m in messages):
                self.send_json({"id": "x", "object": "chat.completion", "model": "mock-model", "choices": [{"index": 0, "finish_reason": "tool_calls",
                                "message": {"role": "assistant", "content": None, "tool_calls": [{"id": "call_1", "type": "function",
                                "function": {"name": "find", "arguments": json.dumps({"text": "Power"})}}]}}], "usage": {"prompt_tokens": 10, "completion_tokens": 5, "total_tokens": 15}})
                return
            self.send_json({"id": "x", "object": "chat.completion", "model": "mock-model", "choices": [{"index": 0, "finish_reason": "stop",
                            "message": {"role": "assistant", "content": "E1 is the part that powers the aircraft. It weighs about 99 kg."}}], "usage": {"prompt_tokens": 10, "completion_tokens": 5, "total_tokens": 15}})
            return
        if "what a system model is missing" in text: answer = GAPS
        elif "how much a change matters" in text:
            answer = {"ratings": [{"n": "I1", "significance": "High", "reason": "Mock answer."}, {"n": "I2", "significance": "Medium", "reason": "Mock answer."}]}
        elif "explain a change to a system model" in text:
            answer = {"technical": "SYS-001 misses its limit: 31 / 35 min. Re-run TC-007, down 16 percent.", "stakeholder": "The smaller battery on C-003 means about 31 minutes in the air, not 35.", "decision": "Keep the current battery, or accept shorter missions."}
        elif "people who aren't engineers" in text:
            answer = {"paragraph": "A quadcopter that photographs an area from a planned route. A few items are still open.", "summaries": []}
        elif "sketch a behaviour scenario" in text:
            answer = {"steps": [{"kind": "message", "from": "E1", "to": "E2", "interface": "IF1", "label": "mock message after 5 s", "has_guard": "yes", "has_timing": "yes"}],
                      "nodes": [], "edges": [], "missing": [{"title": "What cools the battery?", "body": "Mock answer."}]}
        elif "questions about a state machine" in text:
            answer = {"questions": [{"title": "What should happen if the link drops during landing?", "body": "Mock answer.", "confidence": "medium"}]}
        self.send_json({"id": "x", "object": "chat.completion", "model": "mock-model", "choices": [{"index": 0, "finish_reason": "stop",
                        "message": {"role": "assistant", "content": json.dumps(answer)}}], "usage": {"prompt_tokens": 10, "completion_tokens": 5, "total_tokens": 15}})

HTTPServer(("127.0.0.1", int(sys.argv[1]) if len(sys.argv) > 1 else 1234), Handler).serve_forever()
