(ns main-test
  (:require [test-cloudflare-worker :as tu]
            ["node:test" :as t]))

(t/test "GET /"
        (tu/create-assert-fetch-snapshot
         (Request. "http://localhost/")
         "eyJlZmZlY3RzIjpbXSwicmVzcG9uc2UiOiI8aHRtbCA+PGhlYWQgPjxtZXRhICBjaGFyc2V0PSdVVEYtOCc+PC9tZXRhPjxtZXRhICBuYW1lPSd2aWV3cG9ydCcgY29udGVudD0nd2lkdGg9ZGV2aWNlLXdpZHRoLCBpbml0aWFsLXNjYWxlPTEnPjwvbWV0YT48dGl0bGUgPtCg0LXQutC+0LzQtdC90LTQvtCy0LDRgtGMINC90L7QstC+0YHRgtGMPC90aXRsZT48bGluayAgcmVsPSdzdHlsZXNoZWV0JyBocmVmPSdodHRwczovL2Nkbi5qc2RlbGl2ci5uZXQvbnBtL0BwaWNvY3NzL3BpY29AMi9jc3MvcGljby5taW4uY3NzJz48L2xpbms+PC9oZWFkPjxib2R5ID48bWFpbiAgY2xhc3M9J2NvbnRhaW5lcic+PGZvcm0gIG1ldGhvZD0nUE9TVCcgYWN0aW9uPScvc3VibWl0Jz48ZmllbGRzZXQgPjxsYWJlbCA+0KHRgdGL0LvQutCwINC90LAg0L3QvtCy0L7RgdGC0YwsINCx0LjQsdC70LjQvtGC0LXQutGDLCDQvtGC0LfRi9CyINC40LvQuCDQv9GA0LXQtNC70L7QttC10L3QuNC1PGlucHV0ICBuYW1lPSdsaW5rX3RvX2V2ZW50JyByZXF1aXJlZD0ndHJ1ZSc+PC9pbnB1dD48L2xhYmVsPjwvZmllbGRzZXQ+PGlucHV0ICB0eXBlPSdzdWJtaXQnIHZhbHVlPSfQn9GA0LXQtNC70L7QttC40YLRjCc+PC9pbnB1dD48cCA+0JTQu9GPINC60LDQvdCw0LvQsDogPGEgIGhyZWY9J2h0dHBzOi8vdC5tZS9zL2pldHBhY2tfY29tcG9zZScgdGFyZ2V0PSdfYmxhbmsnPkBqZXRwYWNrX2NvbXBvc2U8L2E+PC9wPjwvZm9ybT48L21haW4+PC9ib2R5PjwvaHRtbD4ifQ=="))

(t/test "POST /submit"
        (tu/create-assert-fetch-snapshot
         (Request. "http://localhost/submit"
                   {:method "POST"
                    :headers {"Content-Type" "application/x-www-form-urlencoded"}
                    :body (str "link_to_event=" (encodeURIComponent "https://example.com/news"))})
         "eyJlZmZlY3RzIjpbeyJ1cmwiOiJodHRwczovL2FwaS50ZWxlZ3JhbS5vcmcvYm90dGVzdC10b2tlbi9zZW5kTWVzc2FnZSIsInByb3BzIjp7Im1ldGhvZCI6IlBPU1QiLCJoZWFkZXJzIjp7IkNvbnRlbnQtVHlwZSI6ImFwcGxpY2F0aW9uL2pzb24ifSwiZGVjb2RlciI6Impzb24iLCJib2R5Ijoie1wiY2hhdF9pZFwiOlwidGVzdC1jaGF0XCIsXCJ0ZXh0XCI6XCLQndC+0LLQsNGPINGA0LXQutC+0LzQtdC90LTQsNGG0LjRjyAoY29tcG9zZSBuZXdzKTogaHR0cHM6Ly9leGFtcGxlLmNvbS9uZXdzXCJ9In0sInR5cGUiOiJlZmZlY3RzX3Byb21pc2UuZmV0Y2g6ZmV0Y2gifV0sInJlc3BvbnNlIjoiPGh0bWwgPjxoZWFkID48bWV0YSAgY2hhcnNldD0nVVRGLTgnPjwvbWV0YT48bWV0YSAgbmFtZT0ndmlld3BvcnQnIGNvbnRlbnQ9J3dpZHRoPWRldmljZS13aWR0aCwgaW5pdGlhbC1zY2FsZT0xJz48L21ldGE+PHRpdGxlID7QoNC10LrQvtC80LXQvdC00L7QstCw0YLRjCDQvdC+0LLQvtGB0YLRjDwvdGl0bGU+PGxpbmsgIHJlbD0nc3R5bGVzaGVldCcgaHJlZj0naHR0cHM6Ly9jZG4uanNkZWxpdnIubmV0L25wbS9AcGljb2Nzcy9waWNvQDIvY3NzL3BpY28ubWluLmNzcyc+PC9saW5rPjwvaGVhZD48Ym9keSA+PG1haW4gIGNsYXNzPSdjb250YWluZXInPjxhcnRpY2xlID48cCA+0KHQv9Cw0YHQuNCx0L4hINCh0YHRi9C70LrQsCDQv9C+0LvRg9GH0LXQvdCwOiBodHRwczovL2V4YW1wbGUuY29tL25ld3M8L3A+PGEgIGhyZWY9Jy8nPtCS0LXRgNC90YPRgtGM0YHRjzwvYT48L2FydGljbGU+PC9tYWluPjwvYm9keT48L2h0bWw+In0="))
