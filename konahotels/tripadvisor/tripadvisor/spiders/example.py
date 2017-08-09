# -*- coding: utf-8 -*-
import scrapy
from tripadvisor.items import TripadvisorItem

class TrSpider(scrapy.Spider):
    name = 'trspider'
    start_urls = [
            'https://www.tripadvisor.com/Hotels-g29217-Island_of_Hawaii_Hawaii-Hotels.html'
            ]

    def parse(self, response):
        for href in response.xpath('//div[@class="listing_title"]/a/@href'):
            url = response.urljoin(href.extract())
            yield scrapy.Request(url, callback=self.parse_hotel)

        next_page = response.xpath('//div[@class="unified pagination standard_pagination"]/child::*[2][self::a]/@href')
        if next_page:
            url = response.urljoin(next_page[0].extract())
            yield scrapy.Request(url, self.parse)

    def parse_hotel(self, response):
        for href in response.xpath('//div[starts-with(@class,"quote")]/a/@href'):
            url = response.urljoin(href.extract())
            yield scrapy.Request(url, callback=self.parse_review)


        next_page = response.xpath('//link[@rel="next"]/@href')
        if next_page:
            url = response.urljoin(next_page[0].extract())
            yield scrapy.Request(url, self.parse_hotel)

    def parse_review(self, response):
        item = TripadvisorItem()
        item['headline'] = response.xpath('translate(//div[@class="quote"]/text(),"!"," ")').extract()[0][1:-1]
        item['review'] = response.xpath('translate(//div[@class="entry"]/p,"\n"," ")').extract()[0]
        item['bubbles'] = response.xpath('//span[contains(@class,"ui_bubble_rating")]/@alt').extract()[0]
        item['date'] = response.xpath('normalize-space(//span[contains(@class,"ratingDate")]/@content)').extract()[0]
        item['hotel'] = response.xpath('normalize-space(//span[@class="altHeadInline"]/a/text())').extract()[0]
        return item




