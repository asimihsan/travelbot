from selenium import webdriver
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support.ui import Select
from selenium.webdriver.firefox.firefox_profile import FirefoxProfile
import lxml.html
import pyvirtualdisplay
import platform

class BrowserManager(object):
    def __enter__(self):
        self.browser = Browser()
        return self.browser

    def __exit__(self, type, value, tb):
        if type is not None:
            pass # Exception occurred
        self.browser.close()

class Browser(object):
    def __init__(self):
        self.display = None
        if platform.system() == "Linux":
            self.display = pyvirtualdisplay.Display(visible=0, size=(800, 600))
            self.display.start()

        #self.selenium = webdriver.Chrome()

        firefox_profile = FirefoxProfile()
        firefox_profile.set_preference('permissions.default.stylesheet', 2)
        firefox_profile.set_preference('permissions.default.image', 2)
        firefox_profile.set_preference('dom.ipc.plugins.enabled.libflashplayer.so', 'false')
        self.selenium = webdriver.Firefox(firefox_profile)

    def wait_for_tag_name(self, tag_name):
        WebDriverWait(self.selenium, 5).until(
            lambda driver: driver.find_element_by_tag_name(tag_name))

    def _selenium_wait(self):
        self.wait_for_tag_name('body')

    def close(self):
        self.selenium.quit()
        if self.display:
            self.display.stop()

    def get(self, uri):
        self.selenium.get(uri)
        self._selenium_wait()

    def get_element_by_xpath(self, xpath):
        return self.selenium.find_element_by_xpath(xpath)

    def click_element_by_xpath(self, xpath):
        element = self.get_element_by_xpath(xpath)
        element.click()
        self._selenium_wait()

    def fill_element_by_xpath(self, xpath, contents):
        element = self.get_element_by_xpath(xpath)
        element.clear()
        element.send_keys(contents)

    def select_option_by_xpath(self, xpath, label):
        element = self.get_element_by_xpath(xpath)
        select = Select(element)
        select.select_by_visible_text(label)

    def switch_to_frame(self, frame):
        self.selenium.switch_to_frame(frame)

    def get_page_source_as_doc(self):
        parser = lxml.html.HTMLParser(encoding = 'utf-8')
        return lxml.html.document_fromstring(self.selenium.page_source,
                                             parser = parser)

    def get_cookies(self):
        cookies = self.selenium.get_cookies()
        return dict([(cookie["name"], cookie["value"]) for cookie in cookies])

